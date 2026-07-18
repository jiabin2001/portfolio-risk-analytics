test_that("empirical VaR and ES use positive loss magnitudes", {
  risk <- empirical_var_es(c(-0.10, -0.03, -0.01, 0.01, 0.02), 0.8)
  expect_gte(risk$var, 0)
  expect_gte(risk$es, risk$var)
  expect_equal(risk$return_quantile, -risk$var)
})

test_that("Gaussian analytical values match their formulas", {
  confidence <- 0.99
  risk <- gaussian_var_es(0, 2, confidence)
  expect_equal(risk$var, -2 * qnorm(1 - confidence), tolerance = 1e-12)
  expect_equal(risk$es, 2 * dnorm(qnorm(1 - confidence)) / (1 - confidence), tolerance = 1e-12)
})

test_that("Student-t risk is finite and ordered", {
  risk <- student_t_var_es(0, 1, 6, 0.99)
  expect_true(all(is.finite(unlist(risk[c("var", "es")]))))
  expect_gt(risk$es, risk$var)
})

test_that("invalid confidence and volatility fail clearly", {
  expect_error(empirical_var_es(rnorm(20), 1), "Confidence")
  expect_error(gaussian_var_es(0, 0, 0.99), "positive")
})

test_that("baseline models share a common forecast schema", {
  set.seed(1)
  x <- rnorm(200, sd = 0.02)
  for (model in c("historical", "gaussian", "student_t", "ewma", "filtered_historical")) {
    fit <- forecast_baseline(x, model, 0.99)
    expect_named(fit, c("model", "confidence", "return_quantile", "var", "es", "status",
                        "warnings", "runtime_seconds", "metadata"))
    expect_gte(fit$es, fit$var)
  }
})
