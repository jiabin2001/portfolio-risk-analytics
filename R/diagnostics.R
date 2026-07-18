#' Anderson-Darling statistic for uniformity
#' @param u Values in (0,1).
#' @return Finite test statistic.
#' @export
uniform_ad_statistic <- function(u) {
  u <- sort(as.numeric(u))
  if (!length(u) || any(!is.finite(u)) || any(u <= 0 | u >= 1)) stop("AD input must be finite and strictly inside (0,1).", call. = FALSE)
  n <- length(u)
  i <- seq_len(n)
  -n - mean((2 * i - 1) * (log(u) + log1p(-rev(u))))
}

#' Residual and PIT diagnostics
#' @param fit Successful marginal fit.
#' @param lag Ljung-Box lag.
#' @param pit_epsilon PIT clipping value.
#' @return Named diagnostic list.
#' @export
diagnose_marginal <- function(fit, lag = 10L, pit_epsilon = 1e-8) {
  if (!identical(fit$status, "ok")) stop("Diagnostics require a successful fit.", call. = FALSE)
  z <- as.numeric(fit$standardised_residuals)
  z <- z[is.finite(z)]
  fitdf <- as.integer(fit$spec$arma_p + fit$spec$arma_q)
  lag <- max(fitdf + 1L, min(as.integer(lag), floor(length(z) / 5)))
  residual_lb <- Box.test(z, lag = lag, type = "Ljung-Box", fitdf = fitdf)
  squared_lb <- Box.test(z^2, lag = lag, type = "Ljung-Box", fitdf = 0)
  parameters <- fit$parameters
  pit <- pit_transform(z, fit$spec$distribution, parameters, pit_epsilon)
  ks <- suppressWarnings(stats::ks.test(pit$values, "punif"))
  ad_stat <- uniform_ad_statistic(pit$values)
  ad <- if (requireNamespace("ADGofTest", quietly = TRUE)) {
    test <- ADGofTest::ad.test(pit$values, null = stats::punif)
    list(statistic = unname(test$statistic), p_value = test$p.value, method = "ADGofTest")
  } else list(statistic = ad_stat, p_value = NA_real_, method = "statistic_only")
  list(
    residual_ljung_box_statistic = unname(residual_lb$statistic),
    residual_ljung_box_p_value = residual_lb$p.value,
    squared_ljung_box_statistic = unname(squared_lb$statistic),
    squared_ljung_box_p_value = squared_lb$p.value,
    pit_ks_statistic = unname(ks$statistic), pit_ks_p_value = ks$p.value,
    pit_ad_statistic = ad$statistic, pit_ad_p_value = ad$p_value, pit_ad_method = ad$method,
    pit_values = pit$values, pit_clipped = pit$n_clipped,
    residual_acf = as.numeric(acf(z, plot = FALSE, lag.max = lag)$acf[-1]),
    squared_residual_acf = as.numeric(acf(z^2, plot = FALSE, lag.max = lag)$acf[-1])
  )
}
