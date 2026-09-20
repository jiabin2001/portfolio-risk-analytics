make_comparison_fixture <- function(n = 100L) {
  returns <- rep(c(0.01, -0.01, 0, -0.03, 0.005), length.out = n)
  do.call(rbind, lapply(c("historical", "gaussian"), function(model) data.frame(
    forecast_date = as.Date("2020-01-01") + seq_len(n), model = model,
    confidence = 0.95, var = 0.02, es = 0.04, realised_return = returns,
    runtime_seconds = 0.01, status = "ok", stringsAsFactors = FALSE
  )))
}

test_that("failed forecasts cannot improve comparison by dropping a crash date", {
  forecasts <- make_comparison_fixture()
  forecasts$realised_return <- rep(c(rep(0, 49), -1, rep(0, 50)), 2)
  forecasts$status[150] <- "failed"
  forecasts$var[150] <- NA_real_
  result <- compare_forecast_models(forecasts)
  expect_equal(result$common_observations, c(99L, 99L))
  expect_equal(result$quantile_loss, c(0.001, 0.001))
  expect_equal(result$rank, c(1, 1))
  expect_equal(result$joint_common_observations, c(99L, 99L))
  expect_equal(result$mean_joint_score[[1]], result$mean_joint_score[[2]])
  historical <- result[result$model == "historical", ]
  gaussian <- result[result$model == "gaussian", ]
  expect_equal(historical$own_sample_quantile_loss, 0.0103)
  expect_equal(gaussian$availability, 0.99)
  expect_equal(gaussian$model_failures, 1L)
  expect_equal(gaussian$transition_pairs, 97L)
})

test_that("comparison validates dates, unique keys and consistent realised returns", {
  forecasts <- make_comparison_fixture()
  expect_equal(compare_forecast_models(forecasts), compare_forecast_models(forecasts[nrow(forecasts):1, ]))
  expect_error(compare_forecast_models(rbind(forecasts, forecasts[1, ])), "Duplicate")
  mismatch <- forecasts
  mismatch$realised_return[[1]] <- 0.5
  expect_error(compare_forecast_models(mismatch), "matching realised returns")
  expect_error(compare_forecast_models(forecasts[, names(forecasts) != "forecast_date"]), "forecast_date")
  missing <- forecasts[-150, ]
  result <- compare_forecast_models(missing)
  expect_equal(result$missing_forecast_rows[result$model == "gaussian"], 1L)
  expect_equal(result$transition_pairs[result$model == "gaussian"], 97L)
})

test_that("independence tests never bridge missing observations", {
  expect_equal(exception_transitions(c(FALSE, TRUE, NA, TRUE, FALSE)),
               c(n00 = 0L, n01 = 1L, n10 = 1L, n11 = 0L))
  expect_equal(christoffersen_independence_test(c(FALSE, NA, TRUE, NA, FALSE))$status,
               "insufficient_data")
  expect_equal(christoffersen_independence_test(rep(FALSE, 100))$status,
               "insufficient_transition_states")
  result <- backtest_var(c(0, -0.03, NA, -0.04, 0), 0.02, 0.95)
  expect_equal(result$observations, 4L)
  expect_equal(result$transition_pairs, 2L)
})

test_that("signed loss forecasts determine scores and unsupported FZ0 domains stay explicit", {
  forecasts <- make_comparison_fixture(40)
  forecasts$var <- forecasts$es <- 0
  forecasts$loss_var <- -0.01
  forecasts$loss_es <- -0.005
  forecasts$realised_return <- 0.02
  result <- compare_forecast_models(forecasts)
  expect_equal(result$quantile_loss, rep(0.0005, 2))
  expect_equal(result$common_observations, c(40L, 40L))
  expect_equal(result$joint_common_observations, c(0L, 0L))
  expect_true(all(is.na(result$mean_joint_score)))
  expect_true(all(result$joint_score_status == "unavailable_common_sample"))
})

test_that("joint score comparisons use one common domain-valid sample", {
  forecasts <- make_comparison_fixture(40)
  forecasts$var[[48]] <- forecasts$es[[48]] <- 0
  result <- compare_forecast_models(forecasts)
  expect_equal(result$common_observations, c(40L, 40L))
  expect_equal(result$joint_common_observations, c(39L, 39L))
  expect_equal(result$mean_joint_score[[1]], result$mean_joint_score[[2]])
  expect_true(all(result$joint_score_status == "partial_common_sample"))
})

test_that("paired circular bootstrap is seeded, preserves RNG and identifies equal scores", {
  set.seed(876)
  previous <- .Random.seed
  result <- paired_block_bootstrap(rep(0, 60), 199L, block_length = 4L, seed = 99L)
  expect_identical(.Random.seed, previous)
  expect_equal(result$ci_lower, 0)
  expect_equal(result$ci_upper, 0)
  expect_equal(result$p_value_bootstrap, 1)
  expect_equal(result$status, "ok")
  differences <- sin(seq_len(80) / 5) + 0.1
  first <- paired_block_bootstrap(differences, 199L, 5L, seed = 13L)
  expect_identical(first, paired_block_bootstrap(differences, 199L, 5L, seed = 13L))
  expect_lt(first$ci_lower, first$ci_upper)
  expect_equal(paired_block_bootstrap(1:8, 199L)$status, "insufficient_observations")
})

test_that("block bootstrap resamples contiguous circular blocks rather than iid points", {
  # Every adjacent circular pair in this alternating series sums to zero.
  difference <- rep(c(-1, 1), 15)
  blocks <- paired_block_bootstrap(difference, 99L, 2L, seed = 47L)
  expect_equal(c(blocks$ci_lower, blocks$ci_upper), c(0, 0))
  iid <- paired_block_bootstrap(difference, 99L, 1L, seed = 47L)
  expect_lt(iid$ci_lower, 0)
  expect_gt(iid$ci_upper, 0)
  shifted <- paired_block_bootstrap(difference + 2, 99L, 2L, seed = 47L)
  expect_equal(c(shifted$ci_lower, shifted$ci_upper), c(2, 2))
  expect_equal(shifted$p_value_bootstrap, 0.01)
})

test_that("predictive ability exposes paired date detail and rejects inference across gaps", {
  forecasts <- make_comparison_fixture(40)
  result <- compare_predictive_ability(forecasts, bootstrap_replications = 99L, seed = 52L)
  expect_equal(nrow(result$summary), 2L)
  expect_equal(nrow(result$detail), 80L)
  expect_true(all(result$summary$observations == 40L))
  expect_true(all(result$detail$loss_difference == 0))
  expect_true(all(result$summary$p_value_holm == 1))
  expect_identical(result, compare_predictive_ability(forecasts[nrow(forecasts):1, ],
                                                      bootstrap_replications = 99L, seed = 52L))
  forecasts$status[[60]] <- "failed"
  gap <- compare_predictive_ability(forecasts, bootstrap_replications = 99L)
  expect_true(all(gap$summary$status == "noncontiguous_common_sample"))
  expect_true(all(is.na(gap$summary$ci_lower)))
  expect_true(all(gap$summary$observations == 39L))
  expect_equal(nrow(gap$detail), 78L)
})

test_that("constant training returns give explicit deterministic baseline forecasts", {
  for (model in c("historical", "gaussian", "student_t", "ewma", "filtered_historical", "garch_t")) {
    result <- forecast_baseline(rep(0.01, 100), model, 0.99)
    expect_equal(result$loss_var, -0.01)
    expect_equal(result$loss_es, -0.01)
    expect_equal(result$metadata$engine, "deterministic_point_mass")
    expect_match(result$warnings, "Constant training")
  }
})

test_that("portfolio GARCH-t benchmark uses a conditional Student forecast and records its engine", {
  set.seed(4321)
  returns <- rt(180, df = 7) * 0.012
  result <- forecast_baseline(returns, "garch_t", 0.99, garch_engine = "native")
  expect_equal(result$model, "garch_t")
  expect_equal(result$metadata$engine, "native_two_step")
  expect_gt(result$metadata$df, 2)
  expect_true(is.finite(result$loss_var))
  expect_gte(result$loss_es, result$loss_var)
  expect_equal(result$var, max(0, result$loss_var))
  expect_error(forecast_baseline(returns[1:80], "garch_t", garch_engine = "native"), "insufficient_observations")
})
