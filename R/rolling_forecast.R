#' Resolve rolling forecast origins without look-ahead
#' @param n Total observations.
#' @param initial_window Initial training length.
#' @param evaluation_observations Optional number of final forecasts.
#' @return Integer forecast origins; each is trained through `origin` and scored at `origin + 1`.
#' @export
rolling_origins <- function(n, initial_window, evaluation_observations = NULL) {
  n <- as.integer(n); initial_window <- as.integer(initial_window)
  if (initial_window < 30L || n <= initial_window) stop("Rolling sample must exceed an initial window of at least 30.", call. = FALSE)
  origins <- seq.int(initial_window, n - 1L)
  if (!is.null(evaluation_observations)) origins <- tail(origins, min(length(origins), as.integer(evaluation_observations)))
  origins
}

#' Resolve training indices for an origin
#' @param origin Last in-sample index.
#' @param window_type Expanding or moving.
#' @param window_size Moving-window size.
#' @return Integer training indices.
#' @export
training_indices <- function(origin, window_type = c("expanding", "moving"), window_size = NULL) {
  window_type <- match.arg(window_type)
  if (window_type == "expanding") return(seq_len(origin))
  if (is.null(window_size) || window_size < 30L) stop("Moving window requires `window_size >= 30`.", call. = FALSE)
  seq.int(max(1L, origin - as.integer(window_size) + 1L), origin)
}

#' Rolling benchmark forecasts
#' @param returns Portfolio returns.
#' @param dates Matching dates.
#' @param models Benchmark model names.
#' @param confidence_levels Confidence levels.
#' @param initial_window Initial training observations.
#' @param window_type Moving or expanding.
#' @param window_size Moving-window size.
#' @param evaluation_observations Number of final forecasts.
#' @param checkpoint_path Optional RDS checkpoint.
#' @param resume Reuse completed forecast keys.
#' @return Common-schema forecast data frame.
#' @export
rolling_baseline_forecasts <- function(returns, dates, models, confidence_levels,
                                       initial_window, window_type = "moving", window_size = initial_window,
                                       evaluation_observations = NULL, checkpoint_path = NULL, resume = TRUE) {
  returns <- as.numeric(returns); dates <- as.Date(dates)
  if (length(returns) != length(dates)) stop("Returns and dates must have the same length.", call. = FALSE)
  origins <- rolling_origins(length(returns), initial_window, evaluation_observations)
  existing <- if (resume && !is.null(checkpoint_path) && file.exists(checkpoint_path)) readRDS(checkpoint_path) else NULL
  if (!is.null(existing) && !is.data.frame(existing)) stop("Rolling checkpoint is not a data frame.", call. = FALSE)
  rows <- if (is.null(existing)) list() else split(existing, seq_len(nrow(existing)))
  completed <- if (is.null(existing) || !nrow(existing)) character() else paste(existing$forecast_date, existing$model, existing$confidence, sep = "|")
  for (origin in origins) {
    idx <- training_indices(origin, window_type, window_size)
    train <- returns[idx]
    for (model in models) for (confidence in confidence_levels) {
      key <- paste(as.character(dates[origin + 1L]), model, confidence, sep = "|")
      if (key %in% completed) next
      started <- proc.time()[["elapsed"]]
      forecast <- tryCatch(forecast_baseline(train, model, confidence), error = function(e) e)
      runtime <- unname(proc.time()[["elapsed"]] - started)
      realised <- returns[origin + 1L]
      if (inherits(forecast, "error")) {
        row <- data.frame(
          forecast_date = dates[origin + 1L], training_start = dates[min(idx)], training_end = dates[max(idx)],
          model = model, confidence = confidence, var = NA_real_, es = NA_real_,
          realised_return = realised, realised_loss = -realised, exceedance = NA,
          runtime_seconds = runtime, warning_status = conditionMessage(forecast), status = "failed",
          stringsAsFactors = FALSE
        )
      } else {
        row <- data.frame(
          forecast_date = dates[origin + 1L], training_start = dates[min(idx)], training_end = dates[max(idx)],
          model = model, confidence = confidence, var = forecast$var, es = forecast$es,
          realised_return = realised, realised_loss = -realised,
          exceedance = -realised > forecast$var,
          runtime_seconds = runtime, warning_status = paste(forecast$warnings, collapse = ";"), status = "ok",
          stringsAsFactors = FALSE
        )
      }
      rows[[length(rows) + 1L]] <- row
    }
    if (!is.null(checkpoint_path)) atomic_save_rds(do.call(rbind, rows), checkpoint_path)
  }
  out <- do.call(rbind, rows)
  out[order(out$forecast_date, out$model, out$confidence), , drop = FALSE]
}

#' Rolling copula-GARCH forecasts
#'
#' Model specifications are reselected periodically using only the current training
#' window. Between reselections the selected specifications are refitted, so no future
#' data enter a past forecast.
#' @param asset_log_returns Two-column asset log-return matrix.
#' @param dates Matching dates.
#' @param cfg Validated configuration.
#' @param checkpoint_path Optional checkpoint path.
#' @param resume Resume completed rows.
#' @param progress_callback Optional function called after each completed forecast origin.
#' @return Forecast data frame.
#' @export
rolling_copula_garch_forecasts <- function(asset_log_returns, dates, cfg, checkpoint_path = NULL,
                                           resume = TRUE, progress_callback = NULL) {
  x <- as.matrix(asset_log_returns); dates <- as.Date(dates)
  if (ncol(x) != 2L || nrow(x) != length(dates)) stop("Copula-GARCH rolling forecast currently requires two aligned assets.", call. = FALSE)
  origins <- rolling_origins(nrow(x), cfg$rolling$initial_window, cfg$rolling$evaluation_observations)
  existing <- if (resume && !is.null(checkpoint_path) && file.exists(checkpoint_path)) readRDS(checkpoint_path) else NULL
  rows <- if (is.null(existing)) list() else split(existing, seq_len(nrow(existing)))
  completed_dates <- if (is.null(existing)) as.Date(character()) else as.Date(unique(existing$forecast_date))
  grid <- build_model_grid(cfg$models)
  selected_specs <- NULL; selected_family <- NULL
  reselection_frequency <- cfg$models$reselection_frequency %||% cfg$models$refit_frequency %||% 20L
  for (position in seq_along(origins)) {
    origin <- origins[[position]]
    forecast_date <- dates[origin + 1L]
    if (forecast_date %in% completed_dates) next
    idx <- training_indices(origin, cfg$rolling$window_type, cfg$rolling$window_size)
    started <- proc.time()[["elapsed"]]
    warnings_seen <- character()
    result <- tryCatch({
      reselect <- is.null(selected_specs) || ((position - 1L) %% reselection_frequency == 0L)
      marginal_fits <- vector("list", 2L)
      selections <- vector("list", 2L)
      for (j in seq_len(2L)) {
        candidate_grid <- if (reselect) grid else selected_specs[[j]]
        fits <- fit_marginal_grid(x[idx, j], candidate_grid, engine = "auto", solver = cfg$models$solver,
                                  timeout_seconds = cfg$models$timeout_seconds)
        selection <- select_marginal_model(fits, cfg$models$diagnostic_alpha, cfg$models$information_criterion)
        if (is.null(selection$selected)) stop(sprintf("No marginal model for asset %d.", j), call. = FALSE)
        marginal_fits[[j]] <- selection$selected; selections[[j]] <- selection
        if (isTRUE(selection$fallback)) warnings_seen <- c(warnings_seen, selection$warning)
      }
      if (reselect) selected_specs <- lapply(marginal_fits, function(fit) as.data.frame(fit$spec, stringsAsFactors = FALSE))
      pits <- lapply(marginal_fits, function(fit) pit_transform(fit$standardised_residuals,
        fit$spec$distribution, fit$parameters)$values)
      common_n <- min(lengths(pits)); u <- cbind(tail(pits[[1]], common_n), tail(pits[[2]], common_n))
      families <- if (reselect || is.null(selected_family)) cfg$models$copula_families else selected_family
      copula <- fit_copula_candidates(u, families, cfg$models$information_criterion)
      if (is.null(copula$selected)) stop("No copula model available.", call. = FALSE)
      selected_family <- copula$selected$family
      simulation <- simulate_portfolio_risk(copula$selected, marginal_fits, cfg$portfolio$weights,
        cfg$simulation$rolling_n, cfg$simulation$seed + origin, cfg$simulation$chunk_size,
        cfg$portfolio$return_type, cfg$risk$confidence_levels)
      list(marginals = marginal_fits, copula = copula$selected, risk = simulation$risk,
           reselected = reselect)
    }, error = function(e) e)
    runtime <- unname(proc.time()[["elapsed"]] - started)
    realised_simple <- drop(expm1(x[origin + 1L, , drop = FALSE]) %*% cfg$portfolio$weights)
    realised <- if (cfg$portfolio$return_type == "log") log1p(realised_simple) else realised_simple
    if (inherits(result, "error")) {
      risk_rows <- data.frame(confidence = cfg$risk$confidence_levels, var = NA_real_, es = NA_real_)
      marginal_names <- NA_character_; copula_name <- NA_character_; status <- "failed"
      warnings_seen <- c(warnings_seen, conditionMessage(result)); reselected <- NA
    } else {
      risk_rows <- result$risk
      marginal_names <- paste(vapply(result$marginals, `[[`, character(1), "model_id"), collapse = ";")
      copula_name <- result$copula$family; status <- "ok"; reselected <- result$reselected
    }
    new_rows <- data.frame(
      forecast_date = rep(forecast_date, nrow(risk_rows)), training_start = dates[min(idx)],
      training_end = dates[max(idx)], model = "copula_garch", confidence = risk_rows$confidence,
      var = risk_rows$var, es = risk_rows$es, realised_return = realised, realised_loss = -realised,
      exceedance = ifelse(is.finite(risk_rows$var), -realised > risk_rows$var, NA),
      marginal_models = marginal_names, copula = copula_name, reselected = reselected,
      runtime_seconds = runtime, warning_status = paste(unique(warnings_seen), collapse = ";"), status = status,
      stringsAsFactors = FALSE
    )
    rows <- c(rows, split(new_rows, seq_len(nrow(new_rows))))
    if (!is.null(checkpoint_path)) atomic_save_rds(do.call(rbind, rows), checkpoint_path)
    if (is.function(progress_callback)) {
      progress_callback(list(position = position, total = length(origins),
                             forecast_date = forecast_date, status = status,
                             runtime_seconds = runtime))
    }
  }
  do.call(rbind, rows)
}
