#' Determine VaR exceedances
#' @param realised_returns Signed realised returns.
#' @param var Positive VaR magnitudes.
#' @return Logical vector, `-return > VaR`.
#' @export
var_exceptions <- function(realised_returns, var) {
  if (length(var) == 1L) var <- rep(var, length(realised_returns))
  if (length(realised_returns) != length(var)) stop("Returns and VaR must have equal length.", call. = FALSE)
  ifelse(is.finite(realised_returns) & is.finite(var), -realised_returns > var, NA)
}

#' Kupiec unconditional coverage test
#' @param exceptions Logical exception indicators.
#' @param confidence VaR confidence level.
#' @return Named test-result list.
#' @export
kupiec_test <- function(exceptions, confidence = 0.99) {
  validate_confidence(confidence)
  e <- as.logical(exceptions)
  e <- e[!is.na(e)]
  n <- length(e)
  if (n < 1L) return(list(statistic = NA_real_, p_value = NA_real_, exceptions = 0L, n = 0L, status = "insufficient_data"))
  x <- sum(e)
  p <- 1 - confidence
  phat <- x / n
  log_null <- xlogy(n - x, 1 - p) + xlogy(x, p)
  log_alt <- xlogy(n - x, 1 - phat) + xlogy(x, phat)
  statistic <- max(0, -2 * (log_null - log_alt))
  list(statistic = statistic, p_value = stats::pchisq(statistic, df = 1, lower.tail = FALSE),
       exceptions = x, n = n, expected_rate = p, observed_rate = phat, status = "ok")
}

#' Christoffersen transition counts
#' @param exceptions Logical indicators.
#' @return Named integer vector n00, n01, n10, n11.
#' @export
exception_transitions <- function(exceptions) {
  e <- as.logical(exceptions)
  e <- e[!is.na(e)]
  if (length(e) < 2L) return(c(n00 = 0L, n01 = 0L, n10 = 0L, n11 = 0L))
  previous <- e[-length(e)]
  current <- e[-1]
  c(n00 = sum(!previous & !current), n01 = sum(!previous & current),
    n10 = sum(previous & !current), n11 = sum(previous & current))
}

#' Christoffersen independence test
#' @param exceptions Logical exception indicators.
#' @return Named test-result list.
#' @export
christoffersen_independence_test <- function(exceptions) {
  e <- as.logical(exceptions)
  e <- e[!is.na(e)]
  cells <- exception_transitions(e)
  if (length(e) < 3L) return(list(statistic = NA_real_, p_value = NA_real_, transitions = cells, status = "insufficient_data"))
  n00 <- cells[["n00"]]; n01 <- cells[["n01"]]; n10 <- cells[["n10"]]; n11 <- cells[["n11"]]
  pi0 <- if ((n00 + n01) == 0) 0 else n01 / (n00 + n01)
  pi1 <- if ((n10 + n11) == 0) 0 else n11 / (n10 + n11)
  pi <- (n01 + n11) / sum(cells)
  log_null <- xlogy(n00 + n10, 1 - pi) + xlogy(n01 + n11, pi)
  log_alt <- xlogy(n00, 1 - pi0) + xlogy(n01, pi0) + xlogy(n10, 1 - pi1) + xlogy(n11, pi1)
  statistic <- max(0, -2 * (log_null - log_alt))
  list(statistic = statistic, p_value = stats::pchisq(statistic, 1, lower.tail = FALSE),
       transitions = cells, pi0 = pi0, pi1 = pi1, status = "ok")
}

#' Christoffersen conditional coverage test
#' @param exceptions Logical indicators.
#' @param confidence VaR confidence level.
#' @return Named test-result list.
#' @export
christoffersen_conditional_test <- function(exceptions, confidence = 0.99) {
  uc <- kupiec_test(exceptions, confidence)
  ind <- christoffersen_independence_test(exceptions)
  if (!identical(uc$status, "ok") || !identical(ind$status, "ok")) {
    return(list(statistic = NA_real_, p_value = NA_real_, status = "insufficient_data", unconditional = uc, independence = ind))
  }
  statistic <- uc$statistic + ind$statistic
  list(statistic = statistic, p_value = stats::pchisq(statistic, 2, lower.tail = FALSE),
       status = "ok", unconditional = uc, independence = ind)
}

#' Quantile (pinball) loss for a lower return quantile
#' @param realised_returns Signed returns.
#' @param var Positive VaR magnitudes.
#' @param confidence VaR confidence.
#' @return Numeric loss vector.
#' @export
quantile_loss <- function(realised_returns, var, confidence = 0.99) {
  validate_confidence(confidence)
  q <- -as.numeric(var)
  y <- as.numeric(realised_returns)
  if (length(q) == 1L) q <- rep(q, length(y))
  if (length(y) != length(q)) stop("Returns and VaR must have equal length.", call. = FALSE)
  alpha <- 1 - confidence
  (alpha - as.numeric(y < q)) * (y - q)
}

#' Traffic-light coverage label
#' @param p_value Conditional-coverage p-value.
#' @param exception_rate Observed rate.
#' @param expected_rate Expected rate.
#' @export
coverage_traffic_light <- function(p_value, exception_rate, expected_rate) {
  if (!is.finite(p_value)) return("insufficient")
  ratio <- if (expected_rate > 0) exception_rate / expected_rate else Inf
  if (p_value >= 0.05 && ratio >= 0.5 && ratio <= 1.5) {
    "green"
  } else if (p_value >= 0.01 && ratio >= 0.25 && ratio <= 2) {
    "amber"
  } else {
    "red"
  }
}

#' Complete VaR backtest summary
#' @param realised_returns Signed returns.
#' @param var Positive VaR.
#' @param confidence Confidence level.
#' @export
backtest_var <- function(realised_returns, var, confidence = 0.99) {
  e <- var_exceptions(realised_returns, var)
  valid <- !is.na(e)
  uc <- kupiec_test(e, confidence)
  ind <- christoffersen_independence_test(e)
  cc <- christoffersen_conditional_test(e, confidence)
  loss <- quantile_loss(realised_returns[valid], if (length(var) == 1L) var else var[valid], confidence)
  data.frame(
    observations = sum(valid), exceptions = sum(e, na.rm = TRUE),
    exception_rate = mean(e, na.rm = TRUE), expected_exception_rate = 1 - confidence,
    kupiec_statistic = uc$statistic, kupiec_p_value = uc$p_value,
    independence_statistic = ind$statistic, independence_p_value = ind$p_value,
    conditional_coverage_statistic = cc$statistic, conditional_coverage_p_value = cc$p_value,
    mean_quantile_loss = mean(loss, na.rm = TRUE),
    coverage_status = coverage_traffic_light(cc$p_value, mean(e, na.rm = TRUE), 1 - confidence),
    stringsAsFactors = FALSE
  )
}
