#' Joint VaR/ES score for loss magnitudes
#'
#' Implements a Fissler-Ziegel-class score. `loss`, `var`, and `es` are all positive
#' loss-scale quantities and ES must be at least VaR.
#' @param realised_returns Signed returns.
#' @param var Positive VaR magnitudes.
#' @param es Positive ES magnitudes.
#' @param confidence Confidence level.
#' @return Numeric score vector; lower is better for comparisons on the same sample.
#' @export
joint_var_es_score <- function(realised_returns, var, es, confidence = 0.99) {
  validate_confidence(confidence)
  loss <- -as.numeric(realised_returns)
  var <- rep_len(as.numeric(var), length(loss))
  es <- rep_len(as.numeric(es), length(loss))
  if (any(!is.finite(es)) || any(es <= 0) || any(es + 1e-12 < var)) {
    stop("ES must be finite, positive, and no smaller than VaR.", call. = FALSE)
  }
  alpha <- 1 - confidence
  ((loss - var) * (loss > var)) / (alpha * es) + var / es + log(es) - 1
}

#' Descriptive Expected Shortfall evaluation
#' @param realised_returns Signed realised returns.
#' @param var Positive VaR forecasts.
#' @param es Positive ES forecasts.
#' @param confidence Confidence level.
#' @return One-row descriptive summary. This is not labelled a formal ES test.
#' @export
evaluate_es <- function(realised_returns, var, es, confidence = 0.99) {
  loss <- -as.numeric(realised_returns)
  var <- rep_len(as.numeric(var), length(loss))
  es <- rep_len(as.numeric(es), length(loss))
  valid <- is.finite(loss) & is.finite(var) & is.finite(es)
  exceed <- valid & loss > var
  tail_residual <- loss[exceed] - es[exceed]
  score <- joint_var_es_score(-loss[valid], var[valid], es[valid], confidence)
  data.frame(
    observations = sum(valid), tail_observations = sum(exceed),
    mean_tail_loss = if (any(exceed)) mean(loss[exceed]) else NA_real_,
    mean_es_forecast_on_exceedance = if (any(exceed)) mean(es[exceed]) else NA_real_,
    mean_exceedance_residual = if (length(tail_residual)) mean(tail_residual) else NA_real_,
    mean_joint_score = mean(score), evaluation_type = "descriptive_and_scoring",
    stringsAsFactors = FALSE
  )
}
