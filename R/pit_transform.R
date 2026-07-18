#' Transform standardised residuals to safe PIT values
#' @param residuals Standardised residuals.
#' @param distribution Innovation distribution.
#' @param parameters Distribution parameters such as `df`, `shape`, and `skew`.
#' @param epsilon Boundary clipping value.
#' @return List with PIT values and clipping metadata.
#' @export
pit_transform <- function(residuals, distribution = "norm", parameters = list(), epsilon = 1e-8) {
  z <- as.numeric(residuals)
  if (any(!is.finite(z))) stop("PIT residuals must be finite.", call. = FALSE)
  if (!is.numeric(epsilon) || length(epsilon) != 1L || epsilon <= 0 || epsilon >= 0.5) {
    stop("PIT epsilon must lie between zero and 0.5.", call. = FALSE)
  }
  raw <- if (distribution == "norm") {
    pnorm(z)
  } else if (distribution == "std") {
    df <- parameters$df %||% parameters$shape %||% 8
    if (!is.finite(df) || df <= 2) stop("Student-t PIT requires df greater than two.", call. = FALSE)
    scale <- sqrt((df - 2) / df)
    stats::pt(z / scale, df)
  } else if (requireNamespace("rugarch", quietly = TRUE) && distribution %in% c("sstd", "ged", "sged")) {
    rugarch::pdist(distribution, z, mu = 0, sigma = 1,
                   skew = parameters$skew %||% 1, shape = parameters$shape %||% 8)
  } else stop(sprintf("PIT distribution `%s` is unavailable without rugarch.", distribution), call. = FALSE)
  if (any(!is.finite(raw))) stop("PIT produced non-finite values.", call. = FALSE)
  clipped <- pmin(1 - epsilon, pmax(epsilon, raw))
  list(values = clipped, n_clipped = sum(clipped != raw), epsilon = epsilon,
       distribution = distribution, raw_range = range(raw), safe_range = range(clipped))
}

#' Invert safe PIT values to standardised shocks
#' @param u Uniform values.
#' @param distribution Innovation distribution.
#' @param parameters Distribution parameters.
#' @param epsilon Boundary clipping.
#' @return Numeric shock vector.
#' @export
inverse_pit <- function(u, distribution = "norm", parameters = list(), epsilon = 1e-8) {
  u <- pmin(1 - epsilon, pmax(epsilon, as.numeric(u)))
  if (any(!is.finite(u))) stop("Uniform values must be finite.", call. = FALSE)
  if (distribution == "norm") return(qnorm(u))
  if (distribution == "std") {
    df <- parameters$df %||% parameters$shape %||% 8
    return(sqrt((df - 2) / df) * qt(u, df))
  }
  if (requireNamespace("rugarch", quietly = TRUE) && distribution %in% c("sstd", "ged", "sged")) {
    return(rugarch::qdist(distribution, u, mu = 0, sigma = 1,
                         skew = parameters$skew %||% 1, shape = parameters$shape %||% 8))
  }
  stop(sprintf("Inverse PIT distribution `%s` is unavailable.", distribution), call. = FALSE)
}
