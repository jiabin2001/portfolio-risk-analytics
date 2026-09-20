#' Validate and score an aligned forecast table
#' @keywords internal
prepare_forecast_scores <- function(forecasts) {
  needed <- c("forecast_date", "model", "confidence", "var", "es", "realised_return", "runtime_seconds", "status")
  if (!is.data.frame(forecasts) || !all(needed %in% names(forecasts)) || !nrow(forecasts)) {
    stop("Forecast table requires a nonempty common schema including forecast_date.", call. = FALSE)
  }
  x <- forecasts
  x$forecast_date <- tryCatch(as.Date(x$forecast_date), error = function(e) rep(as.Date(NA), nrow(x)))
  if (anyNA(x$forecast_date) || anyNA(x$model) || any(!nzchar(as.character(x$model))) || anyNA(x$status)) {
    stop("Forecast dates, model names, and status must be present.", call. = FALSE)
  }
  x$model <- as.character(x$model)
  for (confidence in unique(x$confidence)) validate_confidence(confidence)
  key <- paste(x$model, format(x$confidence, digits = 17), x$forecast_date, sep = "|")
  if (anyDuplicated(key)) stop("Duplicate model/confidence/forecast_date keys.", call. = FALSE)
  if (!is.numeric(x$realised_return) || any(!is.finite(x$realised_return))) {
    stop("Every forecast date must have a finite realised return, including failed forecasts.", call. = FALSE)
  }
  consistent <- vapply(split(x$realised_return, x$forecast_date), function(y) {
    max(y) - min(y) <= 1e-12 * (1 + max(abs(y)))
  }, logical(1))
  if (!all(consistent)) stop("Models must have matching realised returns on each forecast date.", call. = FALSE)
  signed <- c("loss_var", "loss_es") %in% names(x)
  if (any(signed) && !all(signed)) stop("Provide both loss_var and loss_es, or neither.", call. = FALSE)
  x$.loss_var <- if (all(signed)) x$loss_var else x$var
  x$.loss_es <- if (all(signed)) x$loss_es else x$es
  if (!is.numeric(x$.loss_var) || !is.numeric(x$.loss_es)) stop("VaR and ES must be numeric.", call. = FALSE)
  x$.var_valid <- x$status == "ok" & is.finite(x$.loss_var)
  x$.es_valid <- x$.var_valid & is.finite(x$.loss_es) & x$.loss_es + 1e-12 >= x$.loss_var
  x$.quantile_loss <- x$.joint_score <- rep(NA_real_, nrow(x))
  for (confidence in unique(x$confidence)) {
    idx <- which(x$confidence == confidence & x$.var_valid)
    x$.quantile_loss[idx] <- quantile_loss(x$realised_return[idx], x$.loss_var[idx], confidence)
    idx <- which(x$confidence == confidence & x$.es_valid & x$.loss_es > 0)
    x$.joint_score[idx] <- joint_var_es_score(x$realised_return[idx], x$.loss_var[idx], x$.loss_es[idx], confidence)
  }
  x <- x[order(x$confidence, x$forecast_date, x$model), , drop = FALSE]
  row.names(x) <- NULL
  x
}

#' Find dates with a finite score for every model
#' @keywords internal
common_score_dates <- function(x, score_column) {
  dates <- lapply(split(x, x$model), function(model) {
    as.character(model$forecast_date[is.finite(model[[score_column]])])
  })
  sort(Reduce(intersect, dates))
}

#' Mean of a finite score sample, with an explicit unavailable value
#' @keywords internal
score_sample_mean <- function(x) if (length(x)) mean(x) else NA_real_

#' Compare rolling forecasts on common dates
#'
#' Coverage and availability describe each model's full forecast calendar. Missing
#' forecasts remain gaps in independence tests. Quantile and joint-score ranks use
#' separate date intersections shared by every model at a confidence level. Ranks
#' describe sample losses, not evidence of statistically significant superiority.
#' @param forecasts Common-schema forecasts with unique model/confidence/date keys.
#' @return Comparison table with coverage, availability, common-sample scores and ranks.
#' @export
compare_forecast_models <- function(forecasts) {
  forecasts <- prepare_forecast_scores(forecasts)
  rows <- list()
  for (confidence in sort(unique(forecasts$confidence))) {
    group <- forecasts[forecasts$confidence == confidence, , drop = FALSE]
    calendar <- sort(unique(group$forecast_date))
    realised <- group$realised_return[match(calendar, group$forecast_date)]
    common_var <- common_score_dates(group, ".quantile_loss")
    common_joint <- common_score_dates(group, ".joint_score")
    for (model in sort(unique(group$model))) {
      x <- group[group$model == model, , drop = FALSE]
      ok <- x$.var_valid
      loss_var <- rep(NA_real_, length(calendar))
      loss_var[match(x$forecast_date[ok], calendar)] <- x$.loss_var[ok]
      bt <- backtest_var(realised, loss_var, confidence)
      es_ok <- x$.es_valid
      exceed <- es_ok & -x$realised_return > x$.loss_var
      var_common <- as.character(x$forecast_date) %in% common_var
      joint_common <- as.character(x$forecast_date) %in% common_joint
      row <- data.frame(
        model = model, confidence = confidence, forecast_dates = length(calendar),
        observations = bt$observations, availability = sum(ok) / length(calendar),
        missing_forecast_rows = length(calendar) - nrow(x), model_failures = length(calendar) - sum(ok),
        exceptions = bt$exceptions, exception_rate = bt$exception_rate,
        expected_exception_rate = 1 - confidence,
        kupiec_p_value = bt$kupiec_p_value, independence_p_value = bt$independence_p_value,
        independence_status = bt$independence_status, transition_pairs = bt$transition_pairs,
        conditional_coverage_p_value = bt$conditional_coverage_p_value,
        coverage_status = bt$coverage_status,
        own_sample_quantile_loss = bt$mean_quantile_loss,
        common_observations = length(common_var),
        quantile_loss = score_sample_mean(x$.quantile_loss[var_common]),
        joint_common_observations = length(common_joint),
        joint_score_observations = sum(is.finite(x$.joint_score)),
        joint_score_availability = sum(is.finite(x$.joint_score)) / length(calendar),
        mean_joint_score = score_sample_mean(x$.joint_score[joint_common]),
        joint_score_status = if (!length(common_joint)) "unavailable_common_sample" else if (length(common_joint) < length(calendar)) "partial_common_sample" else "ok",
        average_var = score_sample_mean(x$var[ok]), average_es = score_sample_mean(x$es[es_ok]),
        average_loss_var = score_sample_mean(x$.loss_var[ok]), average_loss_es = score_sample_mean(x$.loss_es[es_ok]),
        tail_observations = sum(exceed),
        mean_tail_loss = score_sample_mean(-x$realised_return[exceed]),
        mean_es_forecast_on_exceedance = score_sample_mean(x$.loss_es[exceed]),
        mean_exceedance_residual = score_sample_mean(-x$realised_return[exceed] - x$.loss_es[exceed]),
        runtime_seconds = sum(x$runtime_seconds, na.rm = TRUE), stringsAsFactors = FALSE
      )
      rows[[length(rows) + 1L]] <- row
    }
  }
  out <- do.call(rbind, rows)
  out$rank <- ave(out$quantile_loss, out$confidence,
                 FUN = function(x) rank(x, ties.method = "min", na.last = "keep"))
  out$joint_score_rank <- ave(out$mean_joint_score, out$confidence,
                             FUN = function(x) rank(x, ties.method = "min", na.last = "keep"))
  out <- out[order(out$confidence, out$rank, out$model), , drop = FALSE]
  row.names(out) <- NULL
  out
}
