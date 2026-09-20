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
  if (anyNA(dates) || is.unsorted(dates, strictly = TRUE) || any(!is.finite(returns))) {
    stop("Rolling inputs must be finite and dates strictly increasing.", call. = FALSE)
  }
  if (!length(models)) return(empty_rolling_forecasts())
  origins <- rolling_origins(length(returns), initial_window, evaluation_observations)
  fingerprint <- stable_object_md5(list(
    schema = 3L, implementation = implementation_fingerprint(), returns = returns, dates = as.character(dates), models = models,
    confidence_levels = confidence_levels, initial_window = initial_window,
    window_type = window_type, window_size = window_size,
    evaluation_observations = evaluation_observations
  ))
  checkpoint <- if (resume && !is.null(checkpoint_path) && file.exists(checkpoint_path)) readRDS(checkpoint_path) else NULL
  valid_checkpoint <- is.list(checkpoint) && identical(checkpoint$schema, 3L) &&
    identical(checkpoint$fingerprint, fingerprint) && is.data.frame(checkpoint$rows)
  existing <- if (valid_checkpoint) checkpoint$rows else NULL
  if (!is.null(existing) && nrow(existing)) {
    existing <- existing[existing$status == "ok", , drop = FALSE]
  }
  rows <- if (is.null(existing)) list() else split(existing, seq_len(nrow(existing)))
  completed <- if (is.null(existing) || !nrow(existing)) character() else paste(existing$forecast_date, existing$model, existing$confidence, sep = "|")
  for (origin in origins) {
    idx <- training_indices(origin, window_type, window_size)
    train <- returns[idx]
    for (model in models) for (confidence in confidence_levels) {
      key <- paste(as.character(dates[origin + 1L]), model, confidence, sep = "|")
      if (key %in% completed) next
      started <- proc.time()[["elapsed"]]
      forecast <- tryCatch(with_preserved_seed(origin, forecast_baseline(train, model, confidence)), error = function(e) e)
      runtime <- unname(proc.time()[["elapsed"]] - started)
      realised <- returns[origin + 1L]
      if (inherits(forecast, "error")) {
        row <- data.frame(
          forecast_date = dates[origin + 1L], training_start = dates[min(idx)], training_end = dates[max(idx)],
          model = model, confidence = confidence, var = NA_real_, es = NA_real_,
          loss_var = NA_real_, loss_es = NA_real_,
          realised_return = realised, realised_loss = -realised, exceedance = NA,
          runtime_seconds = runtime, warning_status = conditionMessage(forecast), status = "failed",
          stringsAsFactors = FALSE
        )
      } else {
        row <- data.frame(
          forecast_date = dates[origin + 1L], training_start = dates[min(idx)], training_end = dates[max(idx)],
          model = model, confidence = confidence, var = forecast$var, es = forecast$es,
          loss_var = forecast$loss_var, loss_es = forecast$loss_es,
          realised_return = realised, realised_loss = -realised,
          exceedance = -realised > forecast$loss_var,
          runtime_seconds = runtime, warning_status = paste(forecast$warnings, collapse = ";"), status = "ok",
          stringsAsFactors = FALSE
        )
      }
      rows[[length(rows) + 1L]] <- row
    }
    if (!is.null(checkpoint_path)) {
      atomic_save_rds(list(schema = 3L, fingerprint = fingerprint, rows = do.call(rbind, rows)), checkpoint_path)
    }
  }
  out <- do.call(rbind, rows)
  out <- out[order(out$forecast_date, out$model, out$confidence), , drop = FALSE]
  row.names(out) <- NULL
  out
}

#' Empty common-schema rolling result
#' @keywords internal
empty_rolling_forecasts <- function() {
  data.frame(forecast_date = as.Date(character()), training_start = as.Date(character()),
    training_end = as.Date(character()), model = character(), confidence = numeric(),
    var = numeric(), es = numeric(), loss_var = numeric(), loss_es = numeric(),
    realised_return = numeric(), realised_loss = numeric(), exceedance = logical(),
    runtime_seconds = numeric(), warning_status = character(), status = character())
}

#' Fingerprint a rolling copula checkpoint
#' @keywords internal
rolling_copula_fingerprint <- function(x, dates, cfg, origins) {
  stable_object_md5(list(
    schema = 3L, implementation = implementation_fingerprint(),
    returns = x,
    dates = as.character(dates),
    origins = origins,
    portfolio = cfg$portfolio[c("weights", "return_type", "allow_short")],
    risk = cfg$risk$confidence_levels,
    models = cfg$models,
    simulation = cfg$simulation[c("rolling_n", "chunk_size", "seed")],
    rolling = cfg$rolling[c("initial_window", "window_type", "window_size", "evaluation_observations", "copula_ablations")]
  ))
}

#' Read a versioned rolling copula checkpoint
#' @keywords internal
read_rolling_copula_checkpoint <- function(path, fingerprint) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  value <- readRDS(path)
  valid <- is.list(value) && identical(value$schema, 3L) &&
    identical(value$fingerprint, fingerprint) && is.data.frame(value$rows) &&
    is.list(value$state_history)
  if (valid) value else NULL
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
  if (any(!is.finite(x)) || anyNA(dates) || is.unsorted(dates, strictly = TRUE)) {
    stop("Rolling inputs must be finite and dates strictly increasing.", call. = FALSE)
  }
  ablations <- cfg$rolling$copula_ablations %||% character()
  expected_models <- c("copula_garch", if (length(ablations)) paste0("copula_", ablations) else character())
  expected_keys <- as.vector(outer(expected_models, cfg$risk$confidence_levels, paste, sep = "|"))
  origins <- rolling_origins(nrow(x), cfg$rolling$initial_window, cfg$rolling$evaluation_observations)
  fingerprint <- rolling_copula_fingerprint(x, dates, cfg, origins)
  checkpoint <- if (resume) read_rolling_copula_checkpoint(checkpoint_path, fingerprint) else NULL
  existing <- checkpoint$rows %||% NULL
  state_history <- checkpoint$state_history %||% list()
  completed_prefix <- 0L
  if (!is.null(existing) && nrow(existing)) {
    for (position in seq_along(origins)) {
      forecast_date <- dates[origins[[position]] + 1L]
      date_rows <- existing[as.Date(existing$forecast_date) == forecast_date, , drop = FALSE]
      keys <- paste(date_rows$model, date_rows$confidence, sep = "|")
      complete <- nrow(date_rows) == length(expected_keys) && !anyDuplicated(keys) &&
        setequal(keys, expected_keys) && all(is.finite(date_rows$loss_var)) && all(is.finite(date_rows$loss_es)) &&
        all(is.finite(date_rows$realised_return)) &&
        all(date_rows$status == "ok") && !is.null(state_history[[as.character(position)]])
      if (!complete) break
      completed_prefix <- position
    }
    retained_dates <- if (completed_prefix) {
      dates[origins[seq_len(completed_prefix)] + 1L]
    } else {
      as.Date(character())
    }
    existing <- existing[as.Date(existing$forecast_date) %in% retained_dates, , drop = FALSE]
    state_history <- state_history[names(state_history) %in% as.character(seq_len(completed_prefix))]
  }
  rows <- if (is.null(existing)) list() else split(existing, seq_len(nrow(existing)))
  completed_dates <- if (is.null(existing)) as.Date(character()) else as.Date(unique(existing$forecast_date))
  grid <- build_model_grid(cfg$models)
  prior_state <- if (completed_prefix) state_history[[as.character(completed_prefix)]] else NULL
  selected_specs <- prior_state$selected_specs %||% NULL
  selected_family <- prior_state$selected_family %||% NULL
  reselection_frequency <- cfg$models$reselection_frequency
  for (position in seq_along(origins)) {
    origin <- origins[[position]]
    forecast_date <- dates[origin + 1L]
    if (forecast_date %in% completed_dates) next
    idx <- training_indices(origin, cfg$rolling$window_type, cfg$rolling$window_size)
    started <- proc.time()[["elapsed"]]
    warnings_seen <- character()
    result <- tryCatch(with_preserved_seed(cfg$simulation$seed + origin, {
      reselect <- ((position - 1L) %% reselection_frequency == 0L) || is.null(selected_specs)
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
      next_specs <- if (reselect) {
        lapply(marginal_fits, function(fit) as.data.frame(fit$spec, stringsAsFactors = FALSE))
      } else {
        selected_specs
      }
      pits <- lapply(marginal_fits, function(fit) pit_transform(fit$standardised_residuals,
        fit$spec$distribution, fit$parameters)$values)
      common_n <- min(lengths(pits)); u <- cbind(tail(pits[[1]], common_n), tail(pits[[2]], common_n))
      marginal_runtime <- unname(proc.time()[["elapsed"]] - started)
      families <- if (reselect || is.null(selected_family)) cfg$models$copula_families else selected_family
      copula <- fit_copula_candidates(u, families, cfg$models$information_criterion)
      if (is.null(copula$selected)) stop("No copula model available.", call. = FALSE)
      simulation <- simulate_portfolio_risk(copula$selected, marginal_fits, cfg$portfolio$weights,
        cfg$simulation$rolling_n, cfg$simulation$seed + origin, cfg$simulation$chunk_size,
        cfg$portfolio$return_type, cfg$risk$confidence_levels,
        allow_short = cfg$portfolio$allow_short)
      list(marginals = marginal_fits, copula = copula$selected, risk = simulation$risk,
           reselected = reselect, selected_specs = next_specs,
           selected_family = copula$selected$family, u = u, marginal_runtime = marginal_runtime)
    }), error = function(e) e)
    runtime <- unname(proc.time()[["elapsed"]] - started)
    realised_assets <- if (cfg$portfolio$return_type == "log") expm1(x[origin + 1L, , drop = FALSE]) else x[origin + 1L, , drop = FALSE]
    realised_simple <- drop(realised_assets %*% cfg$portfolio$weights)
    realised <- if (cfg$portfolio$return_type == "log") log1p(realised_simple) else realised_simple
    if (inherits(result, "error")) {
      risk_rows <- data.frame(confidence = cfg$risk$confidence_levels, var = NA_real_, es = NA_real_, loss_var = NA_real_, loss_es = NA_real_)
      marginal_names <- NA_character_; copula_name <- NA_character_; status <- "failed"
      warnings_seen <- c(warnings_seen, conditionMessage(result)); reselected <- NA
    } else {
      risk_rows <- result$risk
      selected_specs <- result$selected_specs
      selected_family <- result$selected_family
      marginal_names <- paste(vapply(result$marginals, `[[`, character(1), "model_id"), collapse = ";")
      copula_name <- result$copula$family; status <- "ok"; reselected <- result$reselected
    }
    new_rows <- data.frame(
      forecast_date = rep(forecast_date, nrow(risk_rows)), training_start = dates[min(idx)],
      training_end = dates[max(idx)], model = "copula_garch", confidence = risk_rows$confidence,
      var = risk_rows$var, es = risk_rows$es, realised_return = realised, realised_loss = -realised,
      loss_var = risk_rows$loss_var, loss_es = risk_rows$loss_es,
      exceedance = ifelse(is.finite(risk_rows$loss_var), -realised > risk_rows$loss_var, NA),
      marginal_models = marginal_names, copula = copula_name, reselected = reselected,
      runtime_seconds = runtime, warning_status = paste(unique(warnings_seen), collapse = ";"), status = status,
      stringsAsFactors = FALSE
    )
    for (family in ablations) {
      ablation_started <- proc.time()[["elapsed"]]
      ablation <- if (inherits(result, "error")) result else tryCatch({
        fit <- if (family == "independence") {
          list(family = "gaussian", engine = "native", status = "ok", parameters = list(rho = 0))
        } else {
          candidates <- fit_copula_candidates(result$u, family, cfg$models$information_criterion)
          if (is.null(candidates$selected) || candidates$selected$family != family) {
            stop(sprintf("Requested ablation family %s did not fit; fallback is not an ablation.", family), call. = FALSE)
          }
          candidates$selected
        }
        simulate_portfolio_risk(fit, result$marginals, cfg$portfolio$weights,
          cfg$simulation$rolling_n, cfg$simulation$seed + origin, cfg$simulation$chunk_size,
          cfg$portfolio$return_type, cfg$risk$confidence_levels,
          allow_short = cfg$portfolio$allow_short)$risk
      }, error = function(e) e)
      extra <- new_rows[new_rows$model == "copula_garch", , drop = FALSE]
      extra$model <- paste0("copula_", family)
      extra$copula <- family
      extra$runtime_seconds <- (if (inherits(result, "error")) runtime else result$marginal_runtime) +
        unname(proc.time()[["elapsed"]] - ablation_started)
      if (inherits(ablation, "error")) {
        extra[, c("var", "es", "loss_var", "loss_es")] <- NA_real_
        extra$exceedance <- NA
        extra$status <- "failed"
        extra$warning_status <- conditionMessage(ablation)
      } else {
        extra[, c("var", "es", "loss_var", "loss_es")] <- ablation[, c("var", "es", "loss_var", "loss_es")]
        extra$exceedance <- -realised > extra$loss_var
      }
      new_rows <- rbind(new_rows, extra)
    }
    rows <- c(rows, split(new_rows, seq_len(nrow(new_rows))))
    state_history[[as.character(position)]] <- list(
      selected_specs = selected_specs,
      selected_family = selected_family
    )
    if (!is.null(checkpoint_path)) {
      atomic_save_rds(list(
        schema = 3L,
        fingerprint = fingerprint,
        rows = do.call(rbind, rows),
        state_history = state_history
      ), checkpoint_path)
    }
    if (is.function(progress_callback)) {
      progress_callback(list(position = position, total = length(origins),
                             forecast_date = forecast_date, status = status,
                             runtime_seconds = runtime))
    }
  }
  out <- do.call(rbind, rows)
  out <- out[order(out$forecast_date, out$model, out$confidence), , drop = FALSE]
  row.names(out) <- NULL
  out
}
