#' Empirical lower and upper tail dependence estimates
#' @param u Two-column matrix of uniform observations.
#' @param threshold Tail probability threshold.
#' @return Named list of threshold estimates.
#' @export
empirical_tail_dependence <- function(u, threshold = 0.05) {
  u <- as.matrix(u)
  if (ncol(u) != 2L || any(!is.finite(u)) || any(u <= 0 | u >= 1)) {
    stop("Tail dependence requires a finite two-column matrix inside (0,1).", call. = FALSE)
  }
  if (threshold <= 0 || threshold >= 0.5) stop("Threshold must lie between zero and 0.5.", call. = FALSE)
  list(
    threshold = threshold,
    lower = mean(u[, 1] <= threshold & u[, 2] <= threshold) / threshold,
    upper = mean(u[, 1] >= 1 - threshold & u[, 2] >= 1 - threshold) / threshold,
    interpretation = "Lower tail is joint extreme negative-return dependence on the return-CDF scale."
  )
}

#' Analytical tail dependence for native copulas
#' @param family Copula family.
#' @param parameters Parameter list.
#' @return Named lower and upper coefficients.
#' @export
copula_tail_dependence <- function(family, parameters) {
  rho <- parameters$rho %||% NA_real_
  if (family == "gaussian") return(c(lower = 0, upper = 0))
  if (family == "student") {
    df <- parameters$df
    value <- 2 * stats::pt(-sqrt((df + 1) * (1 - rho) / (1 + rho)), df = df + 1)
    return(c(lower = value, upper = value))
  }
  if (family == "clayton") return(c(lower = 2^(-1 / parameters$theta), upper = 0))
  if (family == "gumbel") return(c(lower = 0, upper = 2 - 2^(1 / parameters$theta)))
  if (family == "frank") return(c(lower = 0, upper = 0))
  c(lower = NA_real_, upper = NA_real_)
}
