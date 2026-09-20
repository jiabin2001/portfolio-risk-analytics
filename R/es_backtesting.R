#' Expand a scalar forecast without silently recycling a shorter vector
#' @keywords internal
expand_risk_forecast <- function(x, n, label) {
  x <- as.numeric(x)
  if (length(x) == 1L) return(rep(x, n))
  if (length(x) != n) stop(sprintf("`%s` must have length one or match realised returns.", label), call. = FALSE)
  x
}

#' Joint VaR/ES score on the signed loss scale
#'
#' Implements the Fissler-Ziegel zero-homogeneous score (FZ0). VaR may be negative,
#' but this score requires strictly positive loss-scale ES. Valid forecasts outside
#' that domain return `NA`, rather than clipping their signed risk or assigning an
#' artificial score. Missing observations also return `NA`. ES must be at least VaR.
#' @param realised_returns Signed returns.
#' @param var Signed loss-scale VaR, preferably the risk estimate's `loss_var`.
#' @param es Signed loss-scale ES, preferably the risk estimate's `loss_es`.
#' @param confidence Confidence level.
#' @return Numeric score vector; lower is better for comparisons on the same sample.
#' @export
joint_var_es_score <- function(realised_returns, var, es, confidence = 0.99) {
  validate_confidence(confidence)
  loss <- -as.numeric(realised_returns)
  var <- expand_risk_forecast(var, length(loss), "var")
  es <- expand_risk_forecast(es, length(loss), "es")
  finite_forecast <- is.finite(var) & is.finite(es)
  if (any(finite_forecast & es + 1e-12 < var)) {
    stop("ES must be no smaller than VaR on the signed loss scale.", call. = FALSE)
  }
  alpha <- 1 - confidence
  eligible <- is.finite(loss) & finite_forecast & es > 0
  score <- rep(NA_real_, length(loss))
  score[eligible] <- pmax(loss[eligible] - var[eligible], 0) / (alpha * es[eligible]) +
    var[eligible] / es[eligible] + log(es[eligible]) - 1
  score
}

#' Descriptive Expected Shortfall evaluation
#' @param realised_returns Signed realised returns.
#' @param var Signed loss-scale VaR forecasts.
#' @param es Signed loss-scale ES forecasts.
#' @param confidence Confidence level.
#' @return One-row descriptive summary. This is not labelled a formal ES test.
#'   `mean_joint_score` is unavailable if any otherwise valid observation is outside
#'   the positive-ES scoring domain; counts and status expose that limitation.
#' @export
evaluate_es <- function(realised_returns, var, es, confidence = 0.99) {
  loss <- -as.numeric(realised_returns)
  var <- expand_risk_forecast(var, length(loss), "var")
  es <- expand_risk_forecast(es, length(loss), "es")
  valid <- is.finite(loss) & is.finite(var) & is.finite(es)
  exceed <- valid & loss > var
  tail_residual <- loss[exceed] - es[exceed]
  score <- joint_var_es_score(-loss[valid], var[valid], es[valid], confidence)
  score_count <- sum(is.finite(score))
  score_status <- if (!any(valid)) "no_valid_observations" else if (score_count == sum(valid)) {
    "ok"
  } else if (score_count == 0L) "outside_positive_es_domain" else "partial_positive_es_domain"
  data.frame(
    observations = sum(valid), tail_observations = sum(exceed),
    mean_tail_loss = if (any(exceed)) mean(loss[exceed]) else NA_real_,
    mean_es_forecast_on_exceedance = if (any(exceed)) mean(es[exceed]) else NA_real_,
    mean_exceedance_residual = if (length(tail_residual)) mean(tail_residual) else NA_real_,
    mean_joint_score = if (score_status == "ok") mean(score) else NA_real_,
    joint_score_observations = score_count,
    joint_score_unavailable = sum(valid) - score_count,
    joint_score_status = score_status,
    evaluation_type = "descriptive_and_scoring",
    stringsAsFactors = FALSE
  )
}
