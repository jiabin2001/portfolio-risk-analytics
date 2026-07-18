#' Bivariate Gaussian copula log-likelihood
#' @keywords internal
gaussian_copula_loglik <- function(u, rho) {
  z <- qnorm(u)
  denominator <- 1 - rho^2
  sum(-0.5 * log(denominator) -
        (rho^2 * (z[, 1]^2 + z[, 2]^2) - 2 * rho * z[, 1] * z[, 2]) / (2 * denominator))
}

#' Bivariate Student-t copula log-likelihood
#' @keywords internal
student_copula_loglik <- function(u, rho, df) {
  z <- cbind(qt(u[, 1], df), qt(u[, 2], df))
  determinant <- 1 - rho^2
  quadratic <- (z[, 1]^2 - 2 * rho * z[, 1] * z[, 2] + z[, 2]^2) / determinant
  log_joint <- lgamma((df + 2) / 2) - lgamma(df / 2) - log(df * pi) -
    0.5 * log(determinant) - ((df + 2) / 2) * log1p(quadratic / df)
  sum(log_joint - dt(z[, 1], df, log = TRUE) - dt(z[, 2], df, log = TRUE))
}

#' Fit one native elliptical copula
#' @keywords internal
fit_native_copula <- function(u, family) {
  if (!family %in% c("gaussian", "student")) stop("Native copula engine supports gaussian and student only.", call. = FALSE)
  rho0 <- max(-0.98, min(0.98, cor(qnorm(u[, 1]), qnorm(u[, 2]))))
  if (family == "gaussian") {
    fit <- optimize(function(rho) -gaussian_copula_loglik(u, rho), c(-0.995, 0.995))
    parameters <- list(rho = fit$minimum)
    loglik <- -fit$objective
    k <- 1L
  } else {
    objective <- function(theta) {
      rho <- 0.995 * tanh(theta[1]); df <- 2.01 + exp(theta[2])
      -student_copula_loglik(u, rho, df)
    }
    fit <- optim(c(atanh(rho0 / 0.995), log(8 - 2.01)), objective, method = "Nelder-Mead")
    parameters <- list(rho = 0.995 * tanh(fit$par[1]), df = 2.01 + exp(fit$par[2]))
    loglik <- -fit$value
    k <- 2L
  }
  tails <- copula_tail_dependence(family, parameters)
  list(family = family, engine = "native", status = "ok", converged = TRUE,
       parameters = parameters, loglik = loglik, aic = -2 * loglik + 2 * k,
       bic = -2 * loglik + log(nrow(u)) * k,
       kendall_tau = 2 / pi * asin(parameters$rho),
       lower_tail = unname(tails[["lower"]]), upper_tail = unname(tails[["upper"]]),
       rotation = 0L, warning = NA_character_)
}

#' Map readable names to VineCopula family codes
#' @keywords internal
vine_family_code <- function(family) {
  codes <- c(gaussian = 1L, student = 2L, clayton = 3L, gumbel = 4L, frank = 5L,
             bb1 = 7L, survival_clayton = 13L, survival_gumbel = 14L,
             clayton_90 = 23L, gumbel_90 = 24L, clayton_270 = 33L, gumbel_270 = 34L,
             survival_bb1 = 17L, bb1_90 = 27L, bb1_270 = 37L)
  code <- unname(codes[[family]])
  if (is.null(code)) stop(sprintf("Unknown VineCopula family: %s", family), call. = FALSE)
  code
}

#' Fit one VineCopula candidate
#' @keywords internal
fit_vine_copula <- function(u, family) {
  code <- vine_family_code(family)
  fit <- VineCopula::BiCopEst(u[, 1], u[, 2], family = code, method = "mle")
  tail <- VineCopula::BiCopPar2TailDep(family = fit$family, par = fit$par, par2 = fit$par2)
  list(family = family, engine = "VineCopula", status = "ok", converged = TRUE,
       parameters = list(par = fit$par, par2 = fit$par2, family_code = fit$family),
       fit = fit, loglik = fit$logLik, aic = fit$AIC,
       bic = -2 * fit$logLik + log(nrow(u)) * ifelse(fit$par2 == 0, 1, 2),
       kendall_tau = VineCopula::BiCopPar2Tau(family = fit$family, par = fit$par, par2 = fit$par2),
       lower_tail = unname(tail$lower), upper_tail = unname(tail$upper),
       rotation = if (code >= 30) 270L else if (code >= 20) 90L else if (code >= 10) 180L else 0L,
       warning = NA_character_)
}

#' Fit and select copula candidates
#' @param u Matrix of PIT observations.
#' @param families Candidate names.
#' @param criterion Information criterion.
#' @param engine `auto`, `VineCopula`, or `native`.
#' @return Selected model and a complete candidate table.
#' @export
fit_copula_candidates <- function(u, families = c("gaussian", "student", "clayton", "gumbel", "frank"),
                                  criterion = c("BIC", "AIC"), engine = c("auto", "VineCopula", "native")) {
  criterion <- match.arg(toupper(criterion), c("BIC", "AIC"))
  engine <- match.arg(engine)
  u <- as.matrix(u)
  if (ncol(u) != 2L || nrow(u) < 30L || any(!is.finite(u)) || any(u <= 0 | u >= 1)) {
    stop("Copula fitting requires at least 30 finite two-dimensional PIT values inside (0,1).", call. = FALSE)
  }
  use_vine <- engine == "VineCopula" || (engine == "auto" && requireNamespace("VineCopula", quietly = TRUE))
  fits <- lapply(families, function(family) {
    value <- tryCatch({
      if (use_vine) fit_vine_copula(u, family) else fit_native_copula(u, family)
    }, error = function(e) list(family = family, engine = if (use_vine) "VineCopula" else "native",
                                status = "failed", converged = FALSE, parameters = list(),
                                loglik = NA_real_, aic = NA_real_, bic = NA_real_, kendall_tau = NA_real_,
                                lower_tail = NA_real_, upper_tail = NA_real_, rotation = NA_integer_,
                                warning = conditionMessage(e)))
    value
  })
  table <- do.call(rbind, lapply(fits, function(x) data.frame(
    family = x$family, engine = x$engine, status = x$status, converged = x$converged,
    loglik = x$loglik, aic = x$aic, bic = x$bic, kendall_tau = x$kendall_tau,
    lower_tail = x$lower_tail, upper_tail = x$upper_tail, rotation = x$rotation,
    warning = x$warning, stringsAsFactors = FALSE
  )))
  score <- table[[tolower(criterion)]]
  eligible <- which(table$status == "ok" & is.finite(score))
  fallback <- FALSE
  if (!length(eligible) && !use_vine) {
    fallback_fit <- fit_native_copula(u, "gaussian")
    fits <- c(fits, list(fallback_fit)); fallback <- TRUE
    eligible <- length(fits)
  }
  if (!length(eligible)) return(list(selected = NULL, fits = fits, table = table, fallback = TRUE,
                                     warning = "No copula candidate converged."))
  selected_index <- eligible[which.min(score[eligible])]
  table$selected <- FALSE
  table$selected[selected_index] <- TRUE
  list(selected = fits[[selected_index]], fits = fits, table = table, fallback = fallback,
       warning = if (fallback) "Gaussian copula fallback selected." else NA_character_,
       empirical_tail = empirical_tail_dependence(u))
}

#' Simulate from a fitted copula
#' @param fit Selected copula fit.
#' @param n Number of observations.
#' @param seed Optional seed.
#' @param antithetic Use antithetic pairs for native elliptical copulas.
#' @return Numeric matrix with values strictly inside (0,1).
#' @export
simulate_copula <- function(fit, n, seed = NULL, antithetic = FALSE) {
  if (!identical(fit$status, "ok")) stop("Cannot simulate an unsuccessful copula fit.", call. = FALSE)
  if (!is.null(seed)) set.seed(seed)
  n <- as.integer(n)
  if (n < 1L) stop("Simulation count must be positive.", call. = FALSE)
  if (fit$engine == "VineCopula") {
    if (antithetic) stop("Antithetic pairing is not enabled for potentially asymmetric VineCopula families.", call. = FALSE)
    u <- VineCopula::BiCopSim(N = n, family = fit$fit$family,
                             par = fit$fit$par, par2 = fit$fit$par2)
  } else {
    draw_n <- if (antithetic) ceiling(n / 2) else n
    rho <- fit$parameters$rho
    z1 <- rnorm(draw_n); z2 <- rho * z1 + sqrt(1 - rho^2) * rnorm(draw_n)
    if (fit$family == "student") {
      scale <- sqrt(rchisq(draw_n, fit$parameters$df) / fit$parameters$df)
      u <- cbind(stats::pt(z1 / scale, fit$parameters$df), stats::pt(z2 / scale, fit$parameters$df))
    } else u <- cbind(pnorm(z1), pnorm(z2))
    if (antithetic) u <- rbind(u, 1 - u)[seq_len(n), , drop = FALSE]
  }
  u[] <- pmin(1 - 1e-12, pmax(1e-12, u))
  u
}
