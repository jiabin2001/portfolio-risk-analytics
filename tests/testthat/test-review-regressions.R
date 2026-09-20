make_native_simulation_inputs <- function() {
  copula <- list(
    family = "gaussian", engine = "native", status = "ok",
    parameters = list(rho = 0.45)
  )
  marginal <- function(id, mean = 0, sigma = 0.06) list(
    model_id = id, status = "ok", spec = list(distribution = "norm"),
    parameters = list(), forecast_mean = mean, forecast_sigma = sigma
  )
  list(copula = copula, marginals = list(marginal("asset_a"), marginal("asset_b", 0.002, 0.08)))
}

test_that("copula risk and realised returns use the configured return space", {
  inputs <- make_native_simulation_inputs()
  set.seed(8128)
  seed_before <- .Random.seed
  result <- simulate_portfolio_risk(
    inputs$copula, inputs$marginals, c(0.5, 0.5), n_simulations = 2000,
    seed = 17, chunk_size = 400, marginal_return_type = "log",
    confidence_levels = c(0.95, 0.99)
  )
  expect_identical(.Random.seed, seed_before)
  expect_equal(result$portfolio_model_returns, log1p(result$portfolio_simple_returns))
  expect_equal(result$risk, risk_by_confidence(result$portfolio_model_returns, c(0.95, 0.99)))
  expect_false(isTRUE(all.equal(
    result$risk$var,
    risk_by_confidence(result$portfolio_simple_returns, c(0.95, 0.99))$var
  )))
})

test_that("simulation enforces the configured short-selling gate", {
  inputs <- make_native_simulation_inputs()
  expect_error(
    simulate_portfolio_risk(inputs$copula, inputs$marginals, c(1.2, -0.2), 1000, allow_short = FALSE),
    "Negative weights"
  )
  expect_silent(simulate_portfolio_risk(
    inputs$copula, inputs$marginals, c(1.2, -0.2), 1000,
    allow_short = TRUE
  ))
})

test_that("native copula fallback is selectable and visible in the table", {
  set.seed(19)
  z1 <- rnorm(180)
  u <- cbind(pnorm(z1), pnorm(0.5 * z1 + sqrt(0.75) * rnorm(180)))
  result <- fit_copula_candidates(u, families = "clayton", engine = "native")
  expect_true(result$fallback)
  expect_equal(result$selected$family, "gaussian")
  expect_equal(nrow(result$table), 2)
  expect_equal(sum(result$table$selected), 1)
  expect_equal(result$table$family[result$table$selected], "gaussian")
})

test_that("seeded fixture and copula generation preserve global RNG state", {
  set.seed(314)
  seed_before <- .Random.seed
  invisible(generate_fixture_prices(n = 120, seed = 99))
  expect_identical(.Random.seed, seed_before)
  inputs <- make_native_simulation_inputs()
  invisible(simulate_copula(inputs$copula, 50, seed = 101))
  expect_identical(.Random.seed, seed_before)
})

test_that("price cache identity covers the complete data request", {
  root <- tempfile("pra-cache-signature-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  cfg_a <- make_test_config()
  cfg_b <- cfg_a
  cfg_b$data$start <- "2019-01-04"
  first <- load_price_data(cfg_a, root)
  second <- load_price_data(cfg_b, root)
  repeated <- load_price_data(cfg_a, root)
  expect_false(first$metadata$cache_hit)
  expect_false(second$metadata$cache_hit)
  expect_true(repeated$metadata$cache_hit)
  expect_false(identical(first$metadata$request_signature, second$metadata$request_signature))
  expect_equal(length(list.files(file.path(root, "data", "cache"), pattern = "[.]rds$")), 2)
})

test_that("data paths recognise UNC roots and stale caches explain refresh recovery", {
  expect_true(is_absolute_data_path("\\\\server\\share\\prices.csv"))
  expect_true(is_absolute_data_path("C:/prices/input.csv"))
  expect_false(is_absolute_data_path("data/input.csv"))

  root <- tempfile("pra-stale-cache-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  cfg <- make_test_config()
  invisible(load_price_data(cfg, root))
  cache_path <- list.files(file.path(root, "data", "cache"), full.names = TRUE)[[1]]
  cached <- readRDS(cache_path)
  cached$metadata$request_signature$start <- "1900-01-01"
  saveRDS(cached, cache_path)
  expect_error(load_price_data(cfg, root), "force_refresh")
  expect_silent(load_price_data(cfg, root, force_refresh = TRUE))
})

test_that("configuration validation rejects deep invalid values", {
  cfg <- make_test_config()
  expect_silent(validate_config(cfg))
  bad_return <- cfg
  bad_return$portfolio$return_type <- "logs"
  expect_error(validate_config(bad_return), "portfolio.return_type")
  bad_rolling <- cfg
  bad_rolling$simulation$rolling_n <- NULL
  expect_error(validate_config(bad_rolling), "simulation.*rolling_n")
  bad_seed <- cfg
  bad_seed$simulation$seed <- -1
  expect_error(validate_config(bad_seed), "simulation.seed")
})

test_that("non-finite diagnostics are recorded rather than crashing selection", {
  fit <- list(
    model_id = "degenerate", engine = "native_two_step", status = "ok",
    converged = TRUE, parameter_valid = TRUE, aic = 1, bic = 2,
    runtime_seconds = 0, standardised_residuals = rep(0, 100), parameters = list(),
    spec = list(arma_p = 0, arma_q = 0, garch_p = 1, garch_q = 1,
                volatility_family = "sGARCH", distribution = "norm")
  )
  result <- suppressWarnings(select_marginal_model(list(fit)))
  expect_false(result$table$diagnostics_passed)
  expect_true(nzchar(result$table$rejection_reason))
  expect_false(is.null(result$selected))
})

test_that("native selection does not compare approximate IC across ARMA orders", {
  set.seed(202)
  residuals <- rnorm(180)
  make_fit <- function(id, arma_p, bic) list(
    model_id = id, engine = "native_two_step", status = "ok", converged = TRUE,
    parameter_valid = TRUE, aic = bic - 1, bic = bic, runtime_seconds = 0,
    standardised_residuals = residuals, parameters = list(),
    spec = list(arma_p = arma_p, arma_q = 0, garch_p = 1, garch_q = 1,
                volatility_family = "sGARCH", distribution = "norm")
  )
  result <- select_marginal_model(
    list(make_fit("arma00", 0, 10), make_fit("arma10", 1, -100)),
    diagnostic_alpha = 0, criterion = "BIC"
  )
  expect_equal(result$selected$model_id, "arma00")
  expect_match(result$warning, "restricted to ARMA\\(0,0\\)")
})

test_that("native Student eGARCH uses the unit-variance Student absolute moment", {
  residuals <- c(0.01, -0.02, 0.015, -0.01)
  parameters <- list(omega = -0.2, alpha = 0.15, gamma = -0.05, beta = 0.8, df = 5)
  result <- native_variance_filter(residuals, "eGARCH", parameters)
  initial_variance <- var(residuals)
  z_first <- residuals[[1]] / sqrt(initial_variance)
  expected_abs <- exp(
    log(2) + 0.5 * log(parameters$df - 2) + lgamma((parameters$df + 1) / 2) -
      log(parameters$df - 1) - 0.5 * log(pi) - lgamma(parameters$df / 2)
  )
  expected_second <- exp(parameters$omega + parameters$alpha * (abs(z_first) - expected_abs) +
                           parameters$gamma * z_first + parameters$beta * log(initial_variance))
  expect_equal(result$variance[[2]], expected_second, tolerance = 1e-12)
})

test_that("rolling copula checkpoints preserve state and log-return scoring", {
  cfg <- make_test_config()
  cfg$risk$confidence_levels <- 0.95
  cfg$rolling$evaluation_observations <- 2
  prices <- generate_fixture_prices(n = 220, seed = 44)
  aligned <- align_prices(validate_prices(prices, 150)$data, cfg$data$tickers)
  returns <- calculate_asset_returns(aligned)
  checkpoint <- tempfile("copula-checkpoint-", fileext = ".rds")
  first <- rolling_copula_garch_forecasts(returns$log, returns$dates, cfg, checkpoint, resume = TRUE)
  saved <- readRDS(checkpoint)
  expect_identical(saved$schema, 3L)
  expect_equal(length(saved$state_history), 2)
  origins <- rolling_origins(nrow(returns$log), cfg$rolling$initial_window, 2)
  realised_simple <- vapply(origins, function(origin) {
    drop(expm1(returns$log[origin + 1L, , drop = FALSE]) %*% cfg$portfolio$weights)
  }, numeric(1))
  expect_equal(first$realised_return, log1p(realised_simple), tolerance = 1e-12)
  modified <- file.info(checkpoint)$mtime
  resumed <- rolling_copula_garch_forecasts(returns$log, returns$dates, cfg, checkpoint, resume = TRUE)
  expect_equal(resumed, first)
  expect_equal(file.info(checkpoint)$mtime, modified)
})

test_that("comparison, ES evaluation, and convergence return complete outputs", {
  set.seed(72)
  realised <- rnorm(80, sd = 0.02)
  forecasts <- rbind(
    data.frame(forecast_date = as.Date("2020-01-01") + seq_along(realised),
               model = "gaussian", confidence = 0.95, var = 0.035, es = 0.045,
               realised_return = realised, runtime_seconds = 0.1, status = "ok"),
    data.frame(forecast_date = as.Date("2020-01-01") + seq_along(realised),
               model = "historical", confidence = 0.95, var = 0.04, es = 0.055,
               realised_return = realised, runtime_seconds = 0.2, status = "ok")
  )
  comparison <- compare_forecast_models(forecasts)
  expect_equal(nrow(comparison), 2)
  expect_true(all(is.finite(comparison$quantile_loss)))
  es <- evaluate_es(realised, rep(0.035, 80), rep(0.045, 80), 0.95)
  expect_equal(es$observations, 80)
  convergence <- monte_carlo_convergence(
    function(n, seed) with_preserved_seed(seed, rnorm(n)),
    counts = c(1000, 2000), seeds = c(11, 29), confidence_levels = 0.95
  )
  expect_equal(nrow(convergence$detail), 4)
  expect_equal(nrow(convergence$summary), 2)
})
