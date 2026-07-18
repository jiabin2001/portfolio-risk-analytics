#' Unit-variance Student-t log density
#' @keywords internal
std_log_density <- function(z, df) {
  scale <- sqrt((df - 2) / df)
  dt(z / scale, df = df, log = TRUE) - log(scale)
}

#' Filter a native conditional variance recursion
#' @keywords internal
native_variance_filter <- function(residuals, family, parameters) {
  e <- as.numeric(residuals)
  n <- length(e)
  h <- rep(var(e), n)
  if (!is.finite(h[1]) || h[1] <= 0) h[1] <- mean(e^2)
  if (family == "sGARCH") {
    for (i in 2:n) h[i] <- parameters$omega + parameters$alpha * e[i - 1]^2 + parameters$beta * h[i - 1]
    next_h <- parameters$omega + parameters$alpha * tail(e, 1)^2 + parameters$beta * tail(h, 1)
  } else if (family == "eGARCH") {
    expectation_abs_z <- sqrt(2 / pi)
    for (i in 2:n) {
      z_prev <- e[i - 1] / sqrt(h[i - 1])
      h[i] <- exp(parameters$omega + parameters$alpha * (abs(z_prev) - expectation_abs_z) +
                    parameters$gamma * z_prev + parameters$beta * log(h[i - 1]))
    }
    z_last <- tail(e, 1) / sqrt(tail(h, 1))
    next_h <- exp(parameters$omega + parameters$alpha * (abs(z_last) - expectation_abs_z) +
                    parameters$gamma * z_last + parameters$beta * log(tail(h, 1)))
  } else stop("Native variance filter supports sGARCH and eGARCH only.", call. = FALSE)
  h[!is.finite(h) | h <= .Machine$double.eps] <- NA_real_
  list(variance = h, forecast = next_h)
}

#' Fit a native two-step ARMA/conditional-volatility model
#' @keywords internal
fit_native_marginal <- function(returns, spec) {
  supported_family <- spec$volatility_family %in% c("sGARCH", "eGARCH")
  supported_distribution <- spec$distribution %in% c("norm", "std")
  if (!supported_family || !supported_distribution || spec$garch_p != 1L || spec$garch_q != 1L) {
    stop("Native engine supports only (1,1) sGARCH/eGARCH with norm or std innovations.", call. = FALSE)
  }
  x <- as.numeric(returns)
  arma <- arima(x, order = c(spec$arma_p, 0L, spec$arma_q), include.mean = TRUE, method = "ML")
  residuals <- as.numeric(residuals(arma))
  residuals <- residuals[is.finite(residuals)]
  if (length(residuals) < 50L) stop("Insufficient finite ARMA residuals.", call. = FALSE)
  variance0 <- var(residuals)
  if (spec$volatility_family == "sGARCH") {
    decode <- function(theta) {
      alpha <- plogis(theta[2]) * 0.999
      beta <- plogis(theta[3]) * (0.999 - alpha)
      list(omega = exp(theta[1]), alpha = alpha, beta = beta,
           df = if (spec$distribution == "std") 2.01 + exp(theta[4]) else NULL)
    }
    initial <- c(log(max(variance0 * 0.05, 1e-10)), qlogis(0.06 / 0.999),
                 qlogis(0.90 / (0.999 - 0.06)))
  } else {
    decode <- function(theta) list(
      omega = theta[1], alpha = exp(theta[2]), gamma = theta[3],
      beta = 0.995 * tanh(theta[4]),
      df = if (spec$distribution == "std") 2.01 + exp(theta[5]) else NULL
    )
    initial <- c(log(variance0) * 0.05, log(0.08), -0.05, atanh(0.90 / 0.995))
  }
  if (spec$distribution == "std") initial <- c(initial, log(8 - 2.01))
  objective <- function(theta) {
    parameters <- decode(theta)
    filtered <- tryCatch(native_variance_filter(residuals, spec$volatility_family, parameters), error = function(e) NULL)
    if (is.null(filtered) || any(!is.finite(filtered$variance))) return(1e20)
    z <- residuals / sqrt(filtered$variance)
    log_density <- if (spec$distribution == "norm") dnorm(z, log = TRUE) else std_log_density(z, parameters$df)
    value <- -sum(log_density - 0.5 * log(filtered$variance))
    if (is.finite(value)) value else 1e20
  }
  fit <- optim(initial, objective, method = "BFGS", control = list(maxit = 1000, reltol = 1e-8), hessian = TRUE)
  parameters <- decode(fit$par)
  filtered <- native_variance_filter(residuals, spec$volatility_family, parameters)
  z <- residuals / sqrt(filtered$variance)
  arma_forecast <- as.numeric(predict(arma, n.ahead = 1)$pred[[1]])
  loglik <- -objective(fit$par)
  k <- length(coef(arma)) + length(fit$par)
  list(
    engine = "native_two_step", converged = fit$convergence == 0L,
    convergence_code = fit$convergence, arma_fit = arma,
    parameters = c(as.list(coef(arma)), parameters),
    standardised_residuals = z, residuals = residuals,
    conditional_sigma = sqrt(filtered$variance),
    forecast_mean = arma_forecast, forecast_sigma = sqrt(filtered$forecast),
    loglik = loglik, aic = -2 * loglik + 2 * k,
    bic = -2 * loglik + log(length(residuals)) * k,
    parameter_valid = all(is.finite(unlist(parameters))) && is.finite(filtered$forecast) && filtered$forecast > 0,
    warnings = "Native engine uses transparent two-step ARMA then volatility estimation; production runs should prefer rugarch."
  )
}

#' Fit a rugarch marginal model
#' @keywords internal
fit_rugarch_marginal <- function(returns, spec, solver = "hybrid") {
  variance <- list(model = spec$volatility_family, garchOrder = c(spec$garch_p, spec$garch_q))
  mean_model <- list(armaOrder = c(spec$arma_p, spec$arma_q), include.mean = TRUE)
  ugarch_spec <- rugarch::ugarchspec(variance.model = variance, mean.model = mean_model,
                                    distribution.model = spec$distribution)
  fit <- rugarch::ugarchfit(spec = ugarch_spec, data = returns, solver = solver,
                           fit.control = list(stationarity = 1, fixed.se = 0))
  convergence <- fit@fit$convergence
  info <- rugarch::infocriteria(fit)
  forecast <- rugarch::ugarchforecast(fit, n.ahead = 1)
  coefficients <- rugarch::coef(fit)
  list(
    engine = "rugarch", converged = identical(as.integer(convergence), 0L),
    convergence_code = convergence, fit = fit, parameters = as.list(coefficients),
    standardised_residuals = as.numeric(rugarch::residuals(fit, standardize = TRUE)),
    residuals = as.numeric(rugarch::residuals(fit, standardize = FALSE)),
    conditional_sigma = as.numeric(rugarch::sigma(fit)),
    forecast_mean = as.numeric(rugarch::fitted(forecast)),
    forecast_sigma = as.numeric(rugarch::sigma(forecast)),
    loglik = as.numeric(rugarch::likelihood(fit)),
    aic = unname(info[[1]]), bic = unname(info[[2]]),
    parameter_valid = all(is.finite(coefficients)), warnings = character()
  )
}

#' Fit one structured marginal model
#' @param returns Numeric return series.
#' @param spec One-row grid data frame or equivalent list.
#' @param engine `auto`, `rugarch`, or `native`.
#' @param solver Rugarch solver.
#' @param min_observations Minimum sample size.
#' @param timeout_seconds Optional per-fit timeout when `R.utils` is available.
#' @return Structured result retaining failures and warnings.
#' @export
fit_marginal_model <- function(returns, spec, engine = c("auto", "rugarch", "native"),
                               solver = "hybrid", min_observations = 100L,
                               timeout_seconds = NULL) {
  engine <- match.arg(engine)
  spec <- as.list(spec)
  model_id <- spec$model_id %||% sprintf("arma%d%d_%s%d%d_%s", spec$arma_p, spec$arma_q,
    tolower(spec$volatility_family), spec$garch_p, spec$garch_q, spec$distribution)
  x <- as.numeric(returns)
  x <- x[is.finite(x)]
  started <- proc.time()[["elapsed"]]
  if (length(x) < min_observations) {
    return(list(model_id = model_id, spec = spec, status = "failed", converged = FALSE,
                parameter_valid = FALSE, error = "insufficient_observations", warnings = character(),
                runtime_seconds = 0))
  }
  selected_engine <- if (engine == "auto") {
    if (requireNamespace("rugarch", quietly = TRUE)) "rugarch" else "native"
  } else engine
  warnings_seen <- character()
  runner <- function() {
    if (selected_engine == "rugarch") fit_rugarch_marginal(x, spec, solver)
    else fit_native_marginal(x, spec)
  }
  value <- tryCatch(
    withCallingHandlers({
      if (!is.null(timeout_seconds) && requireNamespace("R.utils", quietly = TRUE)) {
        R.utils::withTimeout(runner(), timeout = timeout_seconds, onTimeout = "error")
      } else runner()
    }, warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }),
    error = function(e) e
  )
  runtime <- unname(proc.time()[["elapsed"]] - started)
  if (inherits(value, "error")) {
    return(list(model_id = model_id, spec = spec, engine = selected_engine,
                status = "failed", converged = FALSE, parameter_valid = FALSE,
                error = conditionMessage(value), warnings = unique(warnings_seen), runtime_seconds = runtime))
  }
  value$model_id <- model_id
  value$spec <- spec
  value$status <- if (isTRUE(value$converged) && isTRUE(value$parameter_valid)) "ok" else "failed"
  value$error <- if (value$status == "ok") NA_character_ else "non_convergence_or_invalid_parameters"
  value$warnings <- unique(c(value$warnings, warnings_seen))
  value$runtime_seconds <- runtime
  value
}

#' Fit every candidate and retain all results
#' @param returns Numeric returns.
#' @param grid Model grid.
#' @param engine Fit engine.
#' @param solver Solver name.
#' @param timeout_seconds Optional per-fit timeout.
#' @export
fit_marginal_grid <- function(returns, grid, engine = "auto", solver = "hybrid", timeout_seconds = NULL) {
  lapply(seq_len(nrow(grid)), function(i) fit_marginal_model(
    returns, grid[i, , drop = FALSE], engine, solver, timeout_seconds = timeout_seconds
  ))
}
