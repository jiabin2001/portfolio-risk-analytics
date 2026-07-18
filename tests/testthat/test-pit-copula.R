test_that("PIT clipping prevents zero, one, and infinite inverse values", {
  pit <- pit_transform(c(-100, 0, 100), "norm", epsilon = 1e-6)
  expect_true(all(pit$values > 0 & pit$values < 1))
  expect_equal(pit$n_clipped, 2)
  expect_true(all(is.finite(inverse_pit(c(0, 0.5, 1), "norm", epsilon = 1e-6))))
})

test_that("native copula simulation is deterministic and dimensionally valid", {
  set.seed(22)
  z1 <- rnorm(250); z2 <- 0.6 * z1 + 0.8 * rnorm(250)
  u <- cbind(pnorm(z1), pnorm(z2))
  selection <- fit_copula_candidates(u, c("gaussian", "student", "clayton"), engine = "native")
  expect_false(is.null(selection$selected))
  expect_equal(selection$table$status[selection$table$family == "clayton"], "failed")
  a <- simulate_copula(selection$selected, 100, seed = 31)
  b <- simulate_copula(selection$selected, 100, seed = 31)
  expect_equal(a, b)
  expect_equal(dim(a), c(100, 2))
  expect_true(all(a > 0 & a < 1))
})

test_that("tail direction metadata is not reversed", {
  clayton <- copula_tail_dependence("clayton", list(theta = 2))
  gumbel <- copula_tail_dependence("gumbel", list(theta = 2))
  expect_gt(clayton[["lower"]], 0)
  expect_equal(clayton[["upper"]], 0)
  expect_equal(gumbel[["lower"]], 0)
  expect_gt(gumbel[["upper"]], 0)
})
