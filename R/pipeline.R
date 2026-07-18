#' Run the end-to-end portfolio risk analysis
#' @param config_path YAML profile path.
#' @param root Project root.
#' @param run_copula_rolling Override whether the expensive rolling copula model runs.
#' @return Structured analysis result.
#' @export
run_analysis <- function(config_path, root = pra_project_root(), run_copula_rolling = NULL) {
  cfg <- read_config(config_path)
  dirs <- ensure_project_dirs(root)
  run_id <- make_run_id(cfg$profile)
  log_event("INFO", "pipeline", sprintf("Starting %s profile", cfg$profile), run_id, root)
  acquired <- time_stage(load_price_data(cfg, root))
  log_event("INFO", "data", sprintf("Loaded aligned prices in %.3fs; cache_hit=%s",
    acquired$elapsed_seconds, acquired$value$metadata$cache_hit), run_id, root)
  asset_returns <- calculate_asset_returns(acquired$value$aligned)
  local_simple <- apply_currency_mode(asset_returns$simple, cfg$portfolio$currency_mode,
    cfg$portfolio$asset_currencies, cfg$portfolio$base_currency)
  portfolio <- calculate_portfolio_returns(local_simple, cfg$portfolio$weights, cfg$portfolio$allow_short)
  model_returns <- if (cfg$portfolio$return_type == "log") asset_returns$log else local_simple
  portfolio_model_returns <- if (cfg$portfolio$return_type == "log") portfolio$log_return else portfolio$simple_return

  baseline_rows <- do.call(rbind, lapply(c("historical", "gaussian", "student_t", "ewma", "filtered_historical"), function(model) {
    do.call(rbind, lapply(cfg$risk$confidence_levels, function(level) {
      value <- forecast_baseline(portfolio_model_returns, model, level)
      data.frame(model = model, confidence = level, return_quantile = value$return_quantile,
                 var = value$var, es = value$es, runtime_seconds = value$runtime_seconds,
                 status = value$status, stringsAsFactors = FALSE)
    }))
  }))
  baseline_path <- write_output_table(baseline_rows, "current_baseline_risk", root)

  grid <- build_model_grid(cfg$models)
  fingerprint <- analysis_fingerprint(cfg$config_path, model_returns)
  model_stage_path <- file.path(dirs[["cache"]], paste0(cfg$profile, "_current_model_stage.rds"))
  cached_stage <- if (isTRUE(cfg$compute$resume) && file.exists(model_stage_path)) readRDS(model_stage_path) else NULL
  model_cache_hit <- !is.null(cached_stage) && identical(cached_stage$fingerprint, fingerprint)
  if (model_cache_hit) {
    marginal_selections <- cached_stage$marginal_selections
    copula_selection <- cached_stage$copula_selection
    u <- cached_stage$u
    log_event("INFO", "marginals", "Reused dependency-matched current model stage", run_id, root)
  } else {
    marginal_stage <- time_stage(lapply(seq_len(ncol(model_returns)), function(j) {
      fits <- fit_marginal_grid(model_returns[, j], grid, "auto", cfg$models$solver,
                                cfg$models$timeout_seconds)
      select_marginal_model(fits, cfg$models$diagnostic_alpha, cfg$models$information_criterion)
    }))
    marginal_selections <- marginal_stage$value
    log_event("INFO", "marginals", sprintf("Completed %d candidate fits in %.3fs",
      nrow(grid) * ncol(model_returns), marginal_stage$elapsed_seconds), run_id, root)
    if (any(vapply(marginal_selections, function(x) is.null(x$selected), logical(1)))) {
      stop("At least one asset has no valid marginal fallback; see marginal model table.", call. = FALSE)
    }
    selected_marginals <- lapply(marginal_selections, `[[`, "selected")
    pits <- lapply(selected_marginals, function(fit) pit_transform(fit$standardised_residuals,
      fit$spec$distribution, fit$parameters)$values)
    common_n <- min(lengths(pits))
    u <- cbind(tail(pits[[1]], common_n), tail(pits[[2]], common_n))
    copula_selection <- fit_copula_candidates(u, cfg$models$copula_families, cfg$models$information_criterion)
    if (is.null(copula_selection$selected)) stop("No copula fit was available.", call. = FALSE)
    atomic_save_rds(list(fingerprint = fingerprint, marginal_selections = marginal_selections,
                         copula_selection = copula_selection, u = u), model_stage_path)
  }
  marginal_tables <- do.call(rbind, lapply(seq_along(marginal_selections), function(j) {
    table <- marginal_selections[[j]]$table; table$asset <- colnames(model_returns)[j]; table
  }))
  marginal_path <- write_output_table(marginal_tables, "marginal_model_selection", root)
  selected_marginals <- lapply(marginal_selections, `[[`, "selected")
  copula_path <- write_output_table(copula_selection$table, "copula_model_selection", root)
  atomic_save_rds(list(marginal_selections = marginal_selections,
                       copula_selection = copula_selection, fingerprint = fingerprint,
                       cache_hit = model_cache_hit),
                  file.path(dirs[["models"]], "current_model_bundle.rds"))
  simulation <- simulate_portfolio_risk(copula_selection$selected, selected_marginals,
    cfg$portfolio$weights, cfg$simulation$final_n, cfg$simulation$seed,
    cfg$simulation$chunk_size, cfg$portfolio$return_type, cfg$risk$confidence_levels,
    cfg$simulation$keep_asset_returns %||% FALSE,
    allow_short = cfg$portfolio$allow_short)
  simulation_path <- write_output_table(simulation$risk, "current_copula_risk", root)
  atomic_save_rds(simulation$metadata, file.path(dirs[["models"]], "simulation_metadata.rds"))
  log_event("INFO", "simulation", sprintf("Completed %d draws in %.3fs",
    simulation$metadata$n_simulations, simulation$metadata$runtime_seconds), run_id, root)

  checkpoint_path <- if (isTRUE(cfg$rolling$checkpoint)) file.path(dirs[["checkpoints"]], paste0(cfg$profile, "_baseline.rds")) else NULL
  rolling <- rolling_baseline_forecasts(portfolio_model_returns, asset_returns$dates,
    cfg$rolling$models[cfg$rolling$models != "copula_garch"], cfg$risk$confidence_levels,
    cfg$rolling$initial_window, cfg$rolling$window_type, cfg$rolling$window_size,
    cfg$rolling$evaluation_observations, checkpoint_path, cfg$compute$resume)
  run_copula_rolling <- run_copula_rolling %||% ("copula_garch" %in% cfg$rolling$models)
  if (isTRUE(run_copula_rolling)) {
    copula_checkpoint <- if (isTRUE(cfg$rolling$checkpoint)) file.path(dirs[["checkpoints"]], paste0(cfg$profile, "_copula.rds")) else NULL
    progress_callback <- function(progress) {
      if (progress$position == 1L || progress$position %% 5L == 0L || progress$position == progress$total) {
        log_event("INFO", "rolling_copula", sprintf(
          "origin %d/%d date=%s status=%s runtime=%.3fs",
          progress$position, progress$total, progress$forecast_date,
          progress$status, progress$runtime_seconds
        ), run_id, root)
      }
    }
    rolling_copula <- rolling_copula_garch_forecasts(model_returns, asset_returns$dates, cfg,
      copula_checkpoint, cfg$compute$resume, progress_callback)
    missing_cols <- setdiff(names(rolling), names(rolling_copula)); for (name in missing_cols) rolling_copula[[name]] <- NA
    missing_cols <- setdiff(names(rolling_copula), names(rolling)); for (name in missing_cols) rolling[[name]] <- NA
    rolling <- rbind(rolling[, names(rolling_copula)], rolling_copula)
  }
  rolling_path <- write_output_table(rolling, "rolling_forecasts", root)
  comparison <- compare_forecast_models(rolling)
  comparison_path <- write_output_table(comparison, "model_comparison", root)

  convergence <- NULL
  convergence_files <- character()
  if (!is.null(cfg$simulation$convergence_counts) && !is.null(cfg$simulation$convergence_seeds)) {
    simulation_function <- function(n, seed) simulate_portfolio_risk(
      copula_selection$selected, selected_marginals, cfg$portfolio$weights, n, seed,
      cfg$simulation$chunk_size, cfg$portfolio$return_type, cfg$risk$confidence_levels,
      allow_short = cfg$portfolio$allow_short
    )$portfolio_model_returns
    convergence <- monte_carlo_convergence(simulation_function,
      cfg$simulation$convergence_counts, cfg$simulation$convergence_seeds,
      cfg$risk$confidence_levels)
    convergence_files <- c(
      convergence_detail = write_output_table(convergence$detail, "monte_carlo_convergence_detail", root),
      convergence_summary = write_output_table(convergence$summary, "monte_carlo_convergence_summary", root),
      convergence_plot = plot_monte_carlo_convergence(convergence$summary,
        file.path(dirs[["figures"]], "monte_carlo_convergence.png"))
    )
  }

  figure_paths <- c(
    asset_prices = plot_asset_prices(acquired$value$aligned, file.path(dirs[["figures"]], "asset_prices.png")),
    copula_diagnostic = plot_copula_diagnostic(u, simulate_copula(copula_selection$selected, 2000L, cfg$simulation$seed),
      file.path(dirs[["figures"]], "copula_diagnostic.png")),
    simulation = plot_simulation_risk(simulation, file.path(dirs[["figures"]], "simulation_risk.png")),
    var_exceedances = plot_var_exceedances(rolling, file.path(dirs[["figures"]], "rolling_var_exceedances.png"),
      max(cfg$risk$confidence_levels),
      model = if ("copula_garch" %in% rolling$model) "copula_garch" else NULL),
    model_comparison = plot_model_comparison(comparison, file.path(dirs[["figures"]], "model_comparison.png"))
  )
  files <- c(current_baselines = baseline_path, marginal_selection = marginal_path,
             copula_selection = copula_path, current_copula_risk = simulation_path,
             rolling_forecasts = rolling_path, model_comparison = comparison_path,
             convergence_files, figure_paths)
  manifest_path <- write_output_manifest(files, run_id, root)
  log_event("INFO", "pipeline", "Analysis completed", run_id, root)
  list(config = cfg, run_id = run_id, data = acquired$value, asset_returns = asset_returns,
       portfolio_returns = portfolio, baseline_risk = baseline_rows,
       marginal_selections = marginal_selections, copula_selection = copula_selection,
       simulation = simulation, convergence = convergence, rolling = rolling, comparison = comparison,
       files = c(files, manifest = manifest_path))
}
