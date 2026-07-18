test_that("model grids are controlled by configuration", {
  cfg <- make_test_config()
  cfg$models$arma_p <- c(0, 1)
  cfg$models$volatility_families <- c("sGARCH", "eGARCH")
  cfg$models$distributions <- c("norm", "std")
  grid <- build_model_grid(cfg$models)
  expect_equal(nrow(grid), 8)
  expect_true(all(c("model_id", "arma_p", "garch_p", "volatility_family", "distribution") %in% names(grid)))
})

test_that("marginal fits retain success and failure status", {
  set.seed(91)
  x <- rnorm(220, sd = 0.015)
  spec <- data.frame(model_id = "native_test", arma_p = 0, arma_q = 0, garch_p = 1,
                     garch_q = 1, volatility_family = "sGARCH", distribution = "norm")
  fit <- fit_marginal_model(x, spec, engine = "native")
  expect_equal(fit$status, "ok")
  expect_true(is.finite(fit$forecast_sigma))
  unsupported <- spec; unsupported$model_id <- "unsupported"; unsupported$distribution <- "sged"
  failed <- fit_marginal_model(x, unsupported, engine = "native")
  expect_equal(failed$status, "failed")
  expect_match(failed$error, "supports only")
})

test_that("rolling indices and forecast dates prevent look-ahead", {
  set.seed(101)
  y <- rnorm(100, sd = 0.02)
  dates <- as.Date("2020-01-03") + 7 * (0:99)
  forecasts <- rolling_baseline_forecasts(y, dates, c("historical", "gaussian"), 0.95,
    initial_window = 60, window_type = "moving", window_size = 60, evaluation_observations = 8)
  expect_true(all(as.Date(forecasts$training_end) < as.Date(forecasts$forecast_date)))
  expect_equal(nrow(forecasts), 16)
  expect_true(all(forecasts$status == "ok"))
})

test_that("fixture pipeline creates deterministic report inputs", {
  cfg <- make_test_config()
  root <- file.path(tempdir(), paste0("pra-test-", Sys.getpid()))
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(root, "smoke-test.yml")
  yaml::write_yaml(cfg, path)
  device_before <- grDevices::dev.cur()
  result <- run_analysis(path, root = root, run_copula_rolling = FALSE)
  expect_equal(grDevices::dev.cur(), device_before)
  expect_true(all(file.exists(result$files)))
  expect_equal(nrow(result$simulation$risk), 2)
  expect_true(all(result$rolling$status == "ok"))
  expect_true(all(result$comparison$observations == 5))
  resumed <- run_analysis(path, root = root, run_copula_rolling = FALSE)
  bundle <- readRDS(file.path(root, "outputs", "models", "current_model_bundle.rds"))
  expect_true(bundle$cache_hit)
  expect_equal(resumed$simulation$risk, result$simulation$risk)
})
