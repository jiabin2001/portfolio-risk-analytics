#' Compare rolling forecast models
#' @param forecasts Common-schema rolling forecasts.
#' @return Model comparison table.
#' @export
compare_forecast_models <- function(forecasts) {
  needed <- c("model", "confidence", "var", "es", "realised_return", "runtime_seconds", "status")
  if (!all(needed %in% names(forecasts))) stop("Forecast table does not have the common schema.", call. = FALSE)
  groups <- interaction(forecasts$model, forecasts$confidence, drop = TRUE)
  rows <- lapply(split(forecasts, groups), function(x) {
    ok <- x$status == "ok" & is.finite(x$var) & is.finite(x$realised_return)
    confidence <- x$confidence[[1]]
    if (sum(ok) < 1L) return(data.frame(
      model = x$model[[1]], confidence = confidence, observations = 0L,
      exceptions = NA_integer_, exception_rate = NA_real_, kupiec_p_value = NA_real_,
      independence_p_value = NA_real_, conditional_coverage_p_value = NA_real_,
      quantile_loss = NA_real_, average_var = NA_real_, average_es = NA_real_,
      mean_tail_loss = NA_real_, runtime_seconds = sum(x$runtime_seconds, na.rm = TRUE),
      model_failures = sum(!ok), coverage_status = "insufficient", stringsAsFactors = FALSE))
    bt <- backtest_var(x$realised_return[ok], x$var[ok], confidence)
    es_eval <- evaluate_es(x$realised_return[ok], x$var[ok], x$es[ok], confidence)
    data.frame(
      model = x$model[[1]], confidence = confidence, observations = bt$observations,
      exceptions = bt$exceptions, exception_rate = bt$exception_rate,
      kupiec_p_value = bt$kupiec_p_value, independence_p_value = bt$independence_p_value,
      conditional_coverage_p_value = bt$conditional_coverage_p_value,
      quantile_loss = bt$mean_quantile_loss, average_var = mean(x$var[ok]),
      average_es = mean(x$es[ok]), mean_tail_loss = es_eval$mean_tail_loss,
      runtime_seconds = sum(x$runtime_seconds, na.rm = TRUE), model_failures = sum(!ok),
      coverage_status = bt$coverage_status, stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  calibration_penalty <- abs(out$exception_rate - (1 - out$confidence)) / (1 - out$confidence)
  loss_rank <- ave(out$quantile_loss, out$confidence, FUN = function(x) rank(x, ties.method = "average", na.last = "keep"))
  runtime_rank <- ave(out$runtime_seconds, out$confidence, FUN = function(x) rank(x, ties.method = "average", na.last = "keep"))
  complexity <- match(out$model, c("historical", "gaussian", "ewma", "student_t", "filtered_historical", "copula_garch"))
  complexity[is.na(complexity)] <- max(complexity, na.rm = TRUE) + 1
  out$composite_score <- calibration_penalty + 0.5 * loss_rank + 0.1 * runtime_rank + 0.05 * complexity + out$model_failures
  out$rank <- ave(out$composite_score, out$confidence, FUN = function(x) rank(x, ties.method = "min", na.last = "keep"))
  out[order(out$confidence, out$rank, out$model), ]
}
