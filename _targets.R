source("scripts/bootstrap.R")

if (!requireNamespace("targets", quietly = TRUE)) stop("The optional `targets` package is required for this pipeline.")
library(targets)

tar_option_set(packages = character(), format = "rds", error = "continue")

list(
  tar_target(config_file, Sys.getenv("PRA_CONFIG", "config/smoke.yml"), format = "file"),
  tar_target(config, read_config(config_file)),
  tar_target(directories, ensure_project_dirs()),
  tar_target(price_data, load_price_data(config)),
  tar_target(processed_returns, calculate_asset_returns(price_data$aligned)),
  tar_target(portfolio_returns, calculate_portfolio_returns(processed_returns$simple, config$portfolio$weights)),
  tar_target(baseline_models, do.call(rbind, lapply(config$risk$confidence_levels,
    function(level) as.data.frame(forecast_baseline(portfolio_returns$log_return, "historical", level)[c("model", "confidence", "var", "es")])))),
  tar_target(marginal_grid, build_model_grid(config$models)),
  tar_target(marginal_fits, lapply(seq_len(ncol(processed_returns$log)),
    function(j) fit_marginal_grid(processed_returns$log[, j], marginal_grid, solver = config$models$solver))),
  tar_target(selected_models, lapply(marginal_fits,
    select_marginal_model, diagnostic_alpha = config$models$diagnostic_alpha,
    criterion = config$models$information_criterion)),
  tar_target(pit, {
    values <- lapply(selected_models, function(x) pit_transform(x$selected$standardised_residuals,
      x$selected$spec$distribution, x$selected$parameters)$values)
    n <- min(lengths(values)); cbind(tail(values[[1]], n), tail(values[[2]], n))
  }),
  tar_target(copula, fit_copula_candidates(pit, config$models$copula_families,
    config$models$information_criterion)),
  tar_target(simulation, simulate_portfolio_risk(copula$selected, lapply(selected_models, `[[`, "selected"),
    config$portfolio$weights, config$simulation$final_n, config$simulation$seed,
    config$simulation$chunk_size, config$portfolio$return_type, config$risk$confidence_levels)),
  tar_target(risk_metrics, simulation$risk),
  tar_target(rolling_backtest, rolling_baseline_forecasts(portfolio_returns$log_return,
    processed_returns$dates, config$rolling$models[config$rolling$models != "copula_garch"],
    config$risk$confidence_levels, config$rolling$initial_window, config$rolling$window_type,
    config$rolling$window_size, config$rolling$evaluation_observations)),
  tar_target(tables, list(
    risk = write_output_table(risk_metrics, "current_copula_risk"),
    comparison = write_output_table(compare_forecast_models(rolling_backtest), "model_comparison")
  )),
  tar_target(figures, list(
    exceedances = plot_var_exceedances(rolling_backtest, project_path("outputs", "figures", "rolling_var_exceedances.png")),
    copula = plot_copula_diagnostic(pit, simulate_copula(copula$selected, 2000, config$simulation$seed),
      project_path("outputs", "figures", "copula_diagnostic.png"))
  )),
  tar_target(report, {
    if (isTRUE(config$report$render) && nzchar(Sys.which("quarto"))) {
      system2(Sys.which("quarto"), c("render", "report/portfolio_risk_analytics.qmd", "--to", "html"))
    } else "report_inputs_ready"
  })
)
