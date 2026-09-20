test_that("empirical ES uses the exact tail mass including ties", {
  risk <- empirical_var_es(-c(rep(0.01, 99), 0.10), 0.95)
  expect_equal(risk$loss_var, 0.01)
  expect_equal(risk$loss_es, (0.10 + 4 * 0.01) / 5, tolerance = 1e-12)
  expect_equal(risk$tail_observations, 5L)
  expect_equal(risk$tail_effective_observations, 5)

  fractional <- empirical_var_es(-c(1, 2, 3, 4, 10), 0.7)
  expect_equal(fractional$loss_es, (10 + 0.5 * 4) / 1.5)
  expect_equal(fractional$tail_observations, 2L)
  expect_equal(fractional$tail_effective_observations, 1.5)
  expect_equal(empirical_var_es(-c(1, 2, 3, 4, 10), 0.99)$loss_es, 10)
})

test_that("signed risk estimates preserve translation even for gain-only tails", {
  returns <- c(-0.1, -0.03, 0, 0.01, 0.02)
  original <- empirical_var_es(returns, 0.7)
  shifted <- empirical_var_es(returns + 0.2, 0.7)
  expect_equal(shifted$loss_var, original$loss_var - 0.2)
  expect_equal(shifted$loss_es, original$loss_es - 0.2)
  expect_equal(shifted$return_quantile, -shifted$loss_var)
  expect_equal(shifted$var, 0)
  expect_equal(shifted$es, 0)

  for (risk in list(gaussian_var_es(10, 1), student_t_var_es(10, 1, 8))) {
    expect_lt(risk$loss_var, 0)
    expect_lt(risk$loss_es, 0)
    expect_equal(risk$return_quantile, -risk$loss_var)
    expect_equal(risk$var, 0)
    expect_equal(risk$es, 0)
  }
})

test_that("analytical ES agrees with independent quantile integration", {
  confidence <- 0.975
  alpha <- 1 - confidence
  mean_return <- 0.03
  volatility <- 0.2
  gaussian_integral <- -integrate(function(p) {
    mean_return + volatility * qnorm(p)
  }, 0, alpha, rel.tol = 1e-9)$value / alpha
  df <- 5
  student_integral <- -integrate(function(p) {
    mean_return + volatility * sqrt((df - 2) / df) * qt(p, df)
  }, 0, alpha, rel.tol = 1e-9)$value / alpha
  expect_equal(gaussian_var_es(mean_return, volatility, confidence)$loss_es,
               gaussian_integral, tolerance = 1e-8)
  expect_equal(student_t_var_es(mean_return, volatility, df, confidence)$loss_es,
               student_integral, tolerance = 1e-8)
})

test_that("joint scores expose zero and negative ES without discarding valid rows", {
  realised <- c(-0.04, 0.01, 0.02, NA_real_)
  var <- c(0.025, 0, -0.02, 0.025)
  es <- c(0.05, 0, -0.01, 0.05)
  scores <- joint_var_es_score(realised, var, es, 0.95)
  expect_true(is.finite(scores[1]))
  expect_true(all(is.na(scores[2:4])))
  summary <- evaluate_es(realised, var, es, 0.95)
  expect_equal(summary$observations, 3)
  expect_equal(summary$joint_score_observations, 1)
  expect_equal(summary$joint_score_unavailable, 2)
  expect_equal(summary$joint_score_status, "partial_positive_es_domain")
  expect_true(is.na(summary$mean_joint_score))
  zero <- evaluate_es(rep(0, 5), 0, 0, 0.95)
  expect_equal(zero$joint_score_status, "outside_positive_es_domain")
  expect_error(joint_var_es_score(realised, c(0.01, 0.02), 0.05), "length one or match")
  expect_error(joint_var_es_score(0.02, -0.01, -0.02), "no smaller")
})

test_that("FZ0 expected score is minimised by calibrated normal risk", {
  confidence <- 0.95
  risk <- gaussian_var_es(0, 1, confidence)
  expected_score <- function(var, es) integrate(function(y) {
    joint_var_es_score(y, var, es, confidence) * dnorm(y)
  }, -Inf, Inf, rel.tol = 1e-7, subdivisions = 200L)$value
  optimal <- expected_score(risk$loss_var, risk$loss_es)
  expect_equal(optimal, log(risk$loss_es), tolerance = 1e-6)
  expect_gt(expected_score(risk$loss_var, 1.5 * risk$loss_es), optimal)
  expect_gt(expected_score(0.8 * risk$loss_var, risk$loss_es), optimal)
})

test_that("fitted PIT p-values are exploratory and do not veto marginal admission", {
  residuals <- with_preserved_seed(18, rnorm(300) + 1)
  fit <- list(
    model_id = "pit_exploratory", engine = "native_two_step", status = "ok",
    converged = TRUE, parameter_valid = TRUE, aic = 1, bic = 2,
    runtime_seconds = 0, standardised_residuals = residuals, parameters = list(),
    spec = list(arma_p = 0, arma_q = 0, garch_p = 1, garch_q = 1,
                volatility_family = "sGARCH", distribution = "norm")
  )
  diagnostics <- diagnose_marginal(fit)
  expect_lt(diagnostics$pit_ks_p_value, 0.01)
  expect_gt(diagnostics$residual_ljung_box_p_value, 0.01)
  expect_gt(diagnostics$squared_ljung_box_p_value, 0.01)
  expect_equal(diagnostics$pit_p_value_calibration, "exploratory_fitted_parameters")
  selection <- select_marginal_model(list(fit))
  expect_true(selection$table$diagnostics_passed)
  expect_false(selection$fallback)
  expect_equal(selection$table$pit_diagnostic_role, "exploratory_only")
})
