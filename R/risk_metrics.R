#' Validate a confidence level
#' @param confidence Confidence level strictly between zero and one.
#' @return Numeric confidence.
#' @export
validate_confidence <- function(confidence) {
  if (length(confidence) != 1L || !is.numeric(confidence) || !is.finite(confidence) ||
      confidence <= 0 || confidence >= 1) {
    stop("Confidence must be a finite scalar strictly between zero and one.", call. = FALSE)
  }
  confidence
}

#' Empirical Value-at-Risk and Expected Shortfall
#'
#' Returns are signed (negative means loss). VaR and ES are returned as positive loss
#' magnitudes. VaR is a loss-quantile threshold, not a maximum loss.
#' @param returns Numeric returns.
#' @param confidence Confidence level.
#' @param type Quantile algorithm.
#' @return One-row data frame.
#' @export
empirical_var_es <- function(returns, confidence = 0.99, type = 7L) {
  validate_confidence(confidence)
  x <- as.numeric(returns)
  x <- x[is.finite(x)]
  if (length(x) < 2L) stop("At least two finite returns are required.", call. = FALSE)
  losses <- -x
  var_value <- unname(quantile(losses, probs = confidence, type = type))
  tail <- losses[losses >= var_value]
  data.frame(
    confidence = confidence,
    return_quantile = -var_value,
    var = max(0, var_value),
    es = max(0, mean(tail)),
    tail_observations = length(tail),
    method = "empirical",
    stringsAsFactors = FALSE
  )
}

#' Gaussian analytical VaR and ES
#' @param mean_return Conditional mean return.
#' @param volatility Positive conditional standard deviation.
#' @param confidence Confidence level.
#' @return One-row data frame with positive loss magnitudes.
#' @export
gaussian_var_es <- function(mean_return = 0, volatility = 1, confidence = 0.99) {
  validate_confidence(confidence)
  assert_scalar(mean_return, "mean_return", "numeric")
  assert_scalar(volatility, "volatility", "numeric")
  if (volatility <= 0) stop("Volatility must be positive.", call. = FALSE)
  alpha <- 1 - confidence
  q <- mean_return + volatility * qnorm(alpha)
  es_return <- mean_return - volatility * dnorm(qnorm(alpha)) / alpha
  data.frame(confidence = confidence, return_quantile = q,
             var = max(0, -q), es = max(0, -es_return),
             method = "gaussian", stringsAsFactors = FALSE)
}

#' Student-t analytical VaR and ES
#'
#' The Student-t innovation is scaled to unit variance before applying `volatility`.
#' @param mean_return Conditional mean.
#' @param volatility Conditional standard deviation.
#' @param df Degrees of freedom, greater than two.
#' @param confidence Confidence level.
#' @return One-row data frame.
#' @export
student_t_var_es <- function(mean_return = 0, volatility = 1, df = 8, confidence = 0.99) {
  validate_confidence(confidence)
  assert_scalar(mean_return, "mean_return", "numeric")
  assert_scalar(volatility, "volatility", "numeric")
  assert_scalar(df, "df", "numeric")
  if (volatility <= 0 || df <= 2) stop("Volatility must be positive and df greater than two.", call. = FALSE)
  alpha <- 1 - confidence
  tq <- qt(alpha, df)
  scale <- sqrt((df - 2) / df)
  q <- mean_return + volatility * scale * tq
  lower_mean_standard <- -scale * dt(tq, df) * (df + tq^2) / ((df - 1) * alpha)
  es_return <- mean_return + volatility * lower_mean_standard
  data.frame(confidence = confidence, return_quantile = q,
             var = max(0, -q), es = max(0, -es_return), df = df,
             method = "student_t", stringsAsFactors = FALSE)
}

#' Estimate Student-t degrees of freedom by profile likelihood
#' @param standardised_returns Approximately standardised observations.
#' @param bounds Search bounds, both above two.
#' @return Estimated degrees of freedom.
#' @export
estimate_t_df <- function(standardised_returns, bounds = c(2.05, 100)) {
  z <- as.numeric(standardised_returns)
  z <- z[is.finite(z)]
  if (length(z) < 20L) return(8)
  objective <- function(df) {
    scale <- sqrt((df - 2) / df)
    -sum(dt(z / scale, df = df, log = TRUE) - log(scale))
  }
  fit <- optimize(objective, interval = bounds)
  fit$minimum
}

#' Calculate risk measures for multiple confidence levels
#' @param returns Numeric returns.
#' @param confidence_levels Vector of confidence levels.
#' @export
risk_by_confidence <- function(returns, confidence_levels = c(0.95, 0.99)) {
  do.call(rbind, lapply(confidence_levels, function(level) empirical_var_es(returns, level)))
}
