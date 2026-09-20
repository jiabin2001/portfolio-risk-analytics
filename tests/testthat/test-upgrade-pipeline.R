make_native_rolling_runner <- function() {
  runner <- rolling_copula_garch_forecasts
  scope <- new.env(parent = environment(runner))
  environment(runner) <- scope
  fit_grid <- fit_marginal_grid
  fit_copulas <- fit_copula_candidates
  simulate_risk <- simulate_portfolio_risk
  scope$fit_marginal_grid <- function(returns, grid, engine = "auto", solver = "hybrid", timeout_seconds = NULL) {
    fit_grid(returns, grid, engine = "native", solver = solver, timeout_seconds = timeout_seconds)
  }
  scope$fit_copula_candidates <- function(u, families, criterion = "BIC", engine = "auto") {
    fit_copulas(u, families, criterion, engine = "native")
  }
  scope$simulation_calls <- list()
  scope$simulate_portfolio_risk <- function(copula_fit, marginal_fits, ...) {
    scope$simulation_calls[[length(scope$simulation_calls) + 1L]] <- list(
      marginal_models = vapply(marginal_fits, `[[`, character(1), "model_id"),
      marginal_parameters = lapply(marginal_fits, `[[`, "parameters"),
      forecast_mean = vapply(marginal_fits, `[[`, numeric(1), "forecast_mean"),
      forecast_sigma = vapply(marginal_fits, `[[`, numeric(1), "forecast_sigma")
    )
    simulate_risk(copula_fit, marginal_fits, ...)
  }
  list(run = runner, state = scope)
}

make_upgrade_rolling_inputs <- function() {
  cfg <- make_test_config()
  cfg$portfolio$return_type <- "simple"
  cfg$portfolio$weights <- c(0.4, 0.6)
  cfg$rolling$evaluation_observations <- 2L
  cfg$rolling$copula_ablations <- c("independence", "gaussian")
  returns <- with_preserved_seed(1901, {
    first <- rnorm(182, sd = 0.012)
    cbind(asset_a = first, asset_b = 0.6 * first + rnorm(182, sd = 0.01))
  })
  list(cfg = cfg, returns = returns, dates = as.Date("2020-01-03") + 7L * (0:181))
}

test_that("empty baseline requests preserve a bindable forecast schema", {
  dates <- as.Date("2020-01-03") + 7L * (0:39)
  empty <- rolling_baseline_forecasts(rep(0.01, 40), dates, character(), 0.95, 30)
  populated <- rolling_baseline_forecasts(rep(0.01, 40), dates, "historical", 0.95, 30,
                                         evaluation_observations = 1)
  expect_equal(nrow(empty), 0L)
  expect_identical(names(empty), names(populated))
  expect_s3_class(empty$forecast_date, "Date")
  expect_equal(rbind(empty, populated), populated)
  expect_lt(populated$loss_var, 0)
  expect_equal(populated$exceedance, FALSE)
})

test_that("model fingerprints distinguish reordered data and valuation dates", {
  config_path <- tempfile(fileext = ".yml")
  writeLines("profile: fingerprint-regression", config_path)
  x <- matrix(seq_len(80) / 10000, ncol = 2, dimnames = list(NULL, c("a", "b")))
  dates <- as.Date("2020-01-03") + 7L * (0:39)
  fingerprint <- analysis_fingerprint(config_path, x, dates)
  expect_identical(analysis_fingerprint(config_path, x, dates), fingerprint)
  expect_false(identical(analysis_fingerprint(config_path, x[40:1, ], dates), fingerprint))
  expect_false(identical(analysis_fingerprint(config_path, x[, 2:1], dates), fingerprint))
  expect_false(identical(analysis_fingerprint(config_path, x, dates + 1L), fingerprint))
  writeLines("profile: changed-configuration", config_path)
  expect_false(identical(analysis_fingerprint(config_path, x, dates), fingerprint))
})

test_that("future return changes cannot alter an earlier baseline forecast", {
  returns <- with_preserved_seed(816, rnorm(65, sd = 0.02))
  dates <- as.Date("2020-01-03") + 7L * (0:64)
  args <- list(dates = dates, models = c("historical", "gaussian", "student_t", "ewma", "filtered_historical"),
               confidence_levels = c(0.95, 0.99), initial_window = 60,
               evaluation_observations = 2)
  original <- do.call(rolling_baseline_forecasts, c(list(returns = returns), args))
  changed <- returns
  changed[64:65] <- c(-0.3, 0.2)
  perturbed <- do.call(rolling_baseline_forecasts, c(list(returns = changed), args))
  cols <- c("model", "confidence", "var", "es", "loss_var", "loss_es", "status")
  first_date <- min(original$forecast_date)
  expect_equal(original[original$forecast_date == first_date, cols],
               perturbed[perturbed$forecast_date == first_date, cols])
  expect_true(all(perturbed$realised_return[perturbed$forecast_date == first_date] == -0.3))
})

test_that("copula ablations share marginal fits and preserve simple-return scoring and RNG", {
  inputs <- make_upgrade_rolling_inputs()
  runner <- make_native_rolling_runner()
  checkpoint <- tempfile(fileext = ".rds")
  set.seed(829)
  rng_before <- .Random.seed
  forecasts <- runner$run(inputs$returns, inputs$dates, inputs$cfg, checkpoint)
  expect_identical(.Random.seed, rng_before)
  expect_equal(nrow(forecasts), 12L)
  expect_true(all(forecasts$status == "ok"))
  expect_setequal(forecasts$model, c("copula_garch", "copula_independence", "copula_gaussian"))
  expect_equal(length(runner$state$simulation_calls), 6L)
  for (start in c(1L, 4L)) {
    expect_identical(runner$state$simulation_calls[[start]], runner$state$simulation_calls[[start + 1L]])
    expect_identical(runner$state$simulation_calls[[start]], runner$state$simulation_calls[[start + 2L]])
  }
  idx <- match(forecasts$forecast_date, inputs$dates)
  expected_realised <- drop(inputs$returns[idx, , drop = FALSE] %*% inputs$cfg$portfolio$weights)
  expect_equal(forecasts$realised_return, expected_realised, tolerance = 1e-12)
  expect_equal(forecasts$exceedance, -expected_realised > forecasts$loss_var)
  main <- forecasts[forecasts$model == "copula_garch", c("loss_var", "loss_es")]
  gaussian <- forecasts[forecasts$model == "copula_gaussian", c("loss_var", "loss_es")]
  expect_equal(unname(as.matrix(main)), unname(as.matrix(gaussian)), tolerance = 1e-12)
  modified <- file.info(checkpoint)$mtime
  resumed <- runner$run(inputs$returns, inputs$dates, inputs$cfg, checkpoint)
  expect_identical(resumed, forecasts)
  expect_equal(file.info(checkpoint)$mtime, modified)
  expect_equal(length(runner$state$simulation_calls), 6L)

  # Keep the same row count and models while removing one confidence-level key.
  saved <- readRDS(checkpoint)
  rows <- which(saved$rows$model == "copula_gaussian" &
                  saved$rows$forecast_date == min(saved$rows$forecast_date))
  saved$rows$confidence[rows[2L]] <- saved$rows$confidence[rows[1L]]
  saveRDS(saved, checkpoint)
  recovered <- runner$run(inputs$returns, inputs$dates, inputs$cfg, checkpoint)
  key <- function(x) paste(x$forecast_date, x$model, x$confidence)
  expect_equal(anyDuplicated(key(recovered)), 0L)
  expect_setequal(key(recovered), key(forecasts))
  expect_equal(recovered$loss_var, forecasts$loss_var)
  expect_equal(recovered$loss_es, forecasts$loss_es)
})

test_that("future returns cannot change earlier copula or ablation predictions", {
  inputs <- make_upgrade_rolling_inputs()
  runner <- make_native_rolling_runner()
  original <- runner$run(inputs$returns, inputs$dates, inputs$cfg)
  changed <- inputs$returns
  changed[181:182, ] <- matrix(c(-0.12, 0.08, -0.06, 0.03), ncol = 2)
  perturbed <- runner$run(changed, inputs$dates, inputs$cfg)
  first_date <- min(original$forecast_date)
  cols <- c("model", "confidence", "var", "es", "loss_var", "loss_es", "marginal_models", "copula", "status")
  expect_equal(original[original$forecast_date == first_date, cols],
               perturbed[perturbed$forecast_date == first_date, cols])
  expect_true(all(perturbed$realised_return[perturbed$forecast_date == first_date] ==
                    drop(changed[181L, , drop = FALSE] %*% inputs$cfg$portfolio$weights)))
})

test_that("unsupported ablations are recorded as failures without relabelling a fallback", {
  inputs <- make_upgrade_rolling_inputs()
  inputs$cfg$rolling$evaluation_observations <- 1L
  inputs$cfg$rolling$copula_ablations <- "clayton"
  forecasts <- make_native_rolling_runner()$run(inputs$returns, inputs$dates, inputs$cfg)
  expect_true(all(forecasts$status[forecasts$model == "copula_garch"] == "ok"))
  failed <- forecasts[forecasts$model == "copula_clayton", ]
  expect_true(all(failed$status == "failed"))
  expect_true(all(is.na(failed$loss_var) & is.na(failed$loss_es)))
  expect_true(all(grepl("fallback is not an ablation", failed$warning_status)))
})

test_that("run provenance freezes source inputs and records portable artifact checksums", {
  root <- tempfile("pra-provenance-")
  dir.create(root)
  cfg <- make_test_config()
  cfg$config_path <- file.path(root, "source-config.yml")
  writeLines(c("profile: provenance-regression", "data_source: synthetic_fixture"), cfg$config_path)
  dates <- as.Date("2020-01-03") + c(0L, 7L, 14L)
  acquired <- list(
    prices = data.frame(date = rep(dates, 2), ticker = rep(c("a", "b"), each = 3),
                        adjusted = c(100, 101, 102, 50, 51, 52)),
    aligned = data.frame(date = dates, a = c(100, 101, 102), b = c(50, 51, 52)),
    metadata = list(source = "synthetic_fixture", downloaded_at_utc = "2020-01-17 UTC")
  )
  state <- list(git_commit = "regression-source-revision", source_dirty = "FALSE")
  result <- write_run_provenance(cfg, acquired, "unit_run", state, root)
  expect_identical(readRDS(file.path(result$archive, "price_snapshot.rds")), acquired)
  expect_equal(result$values$config_hash, unname(tools::md5sum(cfg$config_path)))
  expect_equal(result$values$input_hash, stable_object_md5(acquired$prices))
  expect_equal(result$values$git_commit, state$git_commit)
  frozen_config <- readLines(file.path(result$archive, "config.yml"))
  writeLines("profile: later-edited-config", cfg$config_path)
  expect_identical(readLines(file.path(result$archive, "config.yml")), frozen_config)
  expect_error(write_run_provenance(cfg, acquired, "unit_run", state, root), "refusing to overwrite")

  table <- write_output_table(data.frame(loss_var = 0.01, loss_es = 0.02), "example_risk", root)
  manifest_path <- write_output_manifest(c(risk = table, provenance = result$path), "unit_run", root,
                                        provenance = state)
  manifest <- read.csv(manifest_path, stringsAsFactors = FALSE)
  expect_true(all(manifest$exists))
  expect_false(any(grepl("^([A-Za-z]:|/)", manifest$path)))
  resolved <- file.path(root, manifest$path)
  expect_equal(manifest$md5, unname(tools::md5sum(resolved)))
  expect_equal(manifest$bytes, file.info(resolved)$size)
  expect_true(all(manifest$git_commit == state$git_commit))
})
