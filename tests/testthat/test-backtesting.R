test_that("exception transition cells are counted exactly", {
  cells <- exception_transitions(c(FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_equal(cells, c(n00 = 1L, n01 = 1L, n10 = 1L, n11 = 1L))
})

test_that("Kupiec handles zero and all exceptions at numerical boundaries", {
  zero <- kupiec_test(rep(FALSE, 250), 0.99)
  all <- kupiec_test(rep(TRUE, 50), 0.99)
  expect_true(is.finite(zero$statistic))
  expect_true(is.finite(all$statistic))
  expect_equal(zero$exceptions, 0)
  expect_equal(all$exceptions, 50)
})

test_that("Christoffersen tests expose insufficient samples", {
  result <- christoffersen_independence_test(TRUE)
  expect_equal(result$status, "insufficient_data")
  cc <- christoffersen_conditional_test(c(FALSE, TRUE), 0.99)
  expect_equal(cc$status, "insufficient_data")
})

test_that("quantile and joint VaR-ES losses are finite", {
  y <- c(-0.04, 0.01, -0.01, -0.03)
  ql <- quantile_loss(y, 0.025, 0.95)
  expect_true(all(is.finite(ql)))
  score <- joint_var_es_score(y, 0.025, 0.05, 0.95)
  expect_true(all(is.finite(score)))
  expect_error(joint_var_es_score(y, 0.05, 0.025, 0.95), "no smaller")
})

test_that("backtest summary uses the loss sign convention", {
  y <- c(-0.04, 0.01, -0.01, -0.03)
  result <- backtest_var(y, rep(0.025, 4), 0.95)
  expect_equal(result$exceptions, 2)
  expect_equal(var_exceptions(y, 0.025), c(TRUE, FALSE, FALSE, TRUE))
})
