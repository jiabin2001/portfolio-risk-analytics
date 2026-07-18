#' EWMA conditional volatility
#' @param returns Numeric returns.
#' @param lambda Decay parameter.
#' @param initial_variance Optional initial variance.
#' @return Variance path including the next-step forecast as an attribute.
#' @export
ewma_variance <- function(returns, lambda = 0.94, initial_variance = NULL) {
  x <- as.numeric(returns)
  if (any(!is.finite(x)) || length(x) < 2L) stop("EWMA requires at least two finite returns.", call. = FALSE)
  if (!is.numeric(lambda) || length(lambda) != 1L || lambda <= 0 || lambda >= 1) {
    stop("EWMA lambda must lie strictly between zero and one.", call. = FALSE)
  }
  variance <- numeric(length(x))
  variance[1] <- initial_variance %||% var(x)
  if (!is.finite(variance[1]) || variance[1] <= 0) variance[1] <- mean(x^2)
  for (i in 2:length(x)) variance[i] <- lambda * variance[i - 1] + (1 - lambda) * x[i - 1]^2
  attr(variance, "forecast") <- lambda * tail(variance, 1) + (1 - lambda) * tail(x, 1)^2
  variance
}

#' Forecast a benchmark VaR/ES model
#' @param returns Training returns.
#' @param model Model name.
#' @param confidence Confidence level.
#' @param ewma_lambda EWMA decay.
#' @return Named list using the common forecast schema.
#' @export
forecast_baseline <- function(returns, model = c("historical", "gaussian", "student_t", "ewma", "filtered_historical"),
                              confidence = 0.99, ewma_lambda = 0.94) {
  model <- match.arg(model)
  validate_confidence(confidence)
  x <- as.numeric(returns)
  x <- x[is.finite(x)]
  if (length(x) < 30L) stop("Benchmark forecast requires at least 30 finite observations.", call. = FALSE)
  started <- proc.time()[["elapsed"]]
  result <- switch(model,
    historical = empirical_var_es(x, confidence),
    gaussian = gaussian_var_es(mean(x), sd(x), confidence),
    student_t = {
      df <- estimate_t_df((x - mean(x)) / sd(x))
      student_t_var_es(mean(x), sd(x), df, confidence)
    },
    ewma = {
      h <- ewma_variance(x, ewma_lambda)
      gaussian_var_es(mean(tail(x, min(20L, length(x)))), sqrt(attr(h, "forecast")), confidence)
    },
    filtered_historical = {
      h <- ewma_variance(x, ewma_lambda)
      z <- x / sqrt(h)
      next_sigma <- sqrt(attr(h, "forecast"))
      risk <- empirical_var_es(z * next_sigma, confidence)
      risk$method <- "filtered_historical"
      risk
    }
  )
  list(
    model = model, confidence = confidence,
    return_quantile = result$return_quantile[[1]],
    var = result$var[[1]], es = result$es[[1]],
    status = "ok", warnings = character(),
    runtime_seconds = unname(proc.time()[["elapsed"]] - started),
    metadata = list(n_training = length(x), ewma_lambda = if (model %in% c("ewma", "filtered_historical")) ewma_lambda else NULL,
                    df = if ("df" %in% names(result)) result$df[[1]] else NULL)
  )
}
