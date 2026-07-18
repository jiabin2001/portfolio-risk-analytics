#' Simulate portfolio returns from copula and marginal forecasts
#' @param copula_fit Successful copula model.
#' @param marginal_fits List of one successful marginal fit per asset.
#' @param weights Portfolio weights.
#' @param n_simulations Number of draws.
#' @param seed Random seed.
#' @param chunk_size Chunk size.
#' @param marginal_return_type Whether marginals model log or simple returns.
#' @param confidence_levels Risk confidence levels.
#' @param keep_asset_returns Retain asset draws (memory intensive).
#' @param antithetic Use native elliptical antithetic pairs.
#' @return Simulation and VaR/ES result object.
#' @export
simulate_portfolio_risk <- function(copula_fit, marginal_fits, weights, n_simulations = 10000L,
                                    seed = 1L, chunk_size = 10000L,
                                    marginal_return_type = c("log", "simple"),
                                    confidence_levels = c(0.95, 0.99),
                                    keep_asset_returns = FALSE, antithetic = FALSE) {
  marginal_return_type <- match.arg(marginal_return_type)
  if (length(marginal_fits) != 2L) stop("Current copula simulation requires exactly two marginal fits.", call. = FALSE)
  if (any(vapply(marginal_fits, function(x) !identical(x$status, "ok"), logical(1)))) {
    stop("All marginal fits must be successful.", call. = FALSE)
  }
  weights <- validate_weights(weights, 2L, allow_short = any(weights < 0))
  n_simulations <- as.integer(n_simulations); chunk_size <- as.integer(chunk_size)
  if (n_simulations < 1000L || chunk_size < 1L) stop("At least 1,000 simulations and a positive chunk size are required.", call. = FALSE)
  started <- proc.time()[["elapsed"]]
  portfolio <- numeric(n_simulations)
  asset_store <- if (keep_asset_returns) matrix(NA_real_, n_simulations, 2L) else NULL
  positions <- split(seq_len(n_simulations), ceiling(seq_len(n_simulations) / chunk_size))
  for (chunk in seq_along(positions)) {
    index <- positions[[chunk]]
    u <- simulate_copula(copula_fit, length(index), seed = seed + chunk - 1L, antithetic = antithetic)
    asset_model_returns <- vapply(seq_len(2L), function(j) {
      fit <- marginal_fits[[j]]
      shocks <- inverse_pit(u[, j], fit$spec$distribution, fit$parameters)
      fit$forecast_mean + fit$forecast_sigma * shocks
    }, numeric(length(index)))
    asset_simple <- if (marginal_return_type == "log") expm1(asset_model_returns) else asset_model_returns
    portfolio[index] <- drop(asset_simple %*% weights)
    if (keep_asset_returns) asset_store[index, ] <- asset_simple
  }
  risk <- do.call(rbind, lapply(confidence_levels, function(level) empirical_var_es(portfolio, level)))
  list(
    portfolio_simple_returns = portfolio,
    asset_simple_returns = asset_store,
    risk = risk,
    metadata = list(n_simulations = n_simulations, seed = seed, chunk_size = chunk_size,
                    antithetic = antithetic, copula_family = copula_fit$family,
                    marginal_models = vapply(marginal_fits, `[[`, character(1), "model_id"),
                    weights = weights, runtime_seconds = unname(proc.time()[["elapsed"]] - started))
  )
}

#' Monte Carlo convergence study
#' @param simulation_function Function accepting `n` and `seed` and returning portfolio returns.
#' @param counts Simulation counts.
#' @param seeds Repeated seeds.
#' @param confidence_levels Risk levels.
#' @return Detailed and summary tables.
#' @export
monte_carlo_convergence <- function(simulation_function,
                                    counts = c(1000, 2000, 5000, 10000, 20000, 50000, 100000),
                                    seeds = c(104729, 130363, 155921),
                                    confidence_levels = c(0.95, 0.99)) {
  rows <- list(); k <- 0L
  for (n in counts) for (seed in seeds) {
    returns <- simulation_function(n = n, seed = seed)
    risk <- risk_by_confidence(returns, confidence_levels)
    risk$n_simulations <- n; risk$seed <- seed
    k <- k + 1L; rows[[k]] <- risk
  }
  detail <- do.call(rbind, rows)
  keys <- interaction(detail$n_simulations, detail$confidence, drop = TRUE)
  summary <- do.call(rbind, lapply(split(detail, keys), function(x) data.frame(
    n_simulations = x$n_simulations[1], confidence = x$confidence[1],
    mean_var = mean(x$var), sd_var = sd(x$var), range_var = diff(range(x$var)),
    mean_es = mean(x$es), sd_es = sd(x$es), range_es = diff(range(x$es)),
    repetitions = nrow(x)
  )))
  summary <- summary[order(summary$confidence, summary$n_simulations), ]
  summary$relative_var_change <- ave(summary$mean_var, summary$confidence, FUN = function(x) c(NA, abs(diff(x) / head(x, -1))))
  summary$relative_es_change <- ave(summary$mean_es, summary$confidence, FUN = function(x) c(NA, abs(diff(x) / head(x, -1))))
  list(detail = detail, summary = summary)
}
