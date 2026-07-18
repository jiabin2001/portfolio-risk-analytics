test_that("price validation reports duplicates and invalid prices", {
  prices <- generate_fixture_prices(n = 120, seed = 11)
  duplicate <- prices[1, ]
  invalid <- data.frame(date = max(prices$date) + 7, ticker = "ASSET_A", adjusted = NA_real_)
  checked <- validate_prices(rbind(prices, duplicate, invalid), min_observations = 100)
  expect_equal(checked$diagnostics$duplicate_count, 1)
  expect_equal(checked$diagnostics$invalid_count, 1)
  expect_true(all(checked$data$adjusted > 0))
})

test_that("alignment keeps common dates and returns use exact definitions", {
  prices <- generate_fixture_prices(n = 120, seed = 12)
  aligned <- align_prices(validate_prices(prices, 100)$data, c("ASSET_A", "ASSET_B"))
  returns <- calculate_asset_returns(aligned)
  expect_equal(nrow(returns$simple), nrow(aligned) - 1)
  expect_equal(returns$log, log1p(returns$simple), tolerance = 1e-12)
  portfolio <- calculate_portfolio_returns(returns$simple, c(0.25, 0.75))
  expect_equal(portfolio$simple_return, 0.25 * returns$simple[, 1] + 0.75 * returns$simple[, 2])
  expect_equal(portfolio$log_return, log1p(portfolio$simple_return))
})

test_that("weights and currency modes are explicit", {
  expect_error(validate_weights(c(0.4, 0.4), 2), "sum to one")
  expect_error(validate_weights(c(1.2, -0.2), 2), "Negative")
  expect_equal(validate_weights(c(1.2, -0.2), 2, allow_short = TRUE), c(1.2, -0.2))
  x <- matrix(c(0.1, 0.2, -0.1, 0.05), ncol = 2)
  local <- apply_currency_mode(x, "local_index_return", c("GBP", "USD"), "GBP")
  expect_false(attr(local, "currency_metadata")$fx_conversion)
  expect_error(apply_currency_mode(x, "base_currency", c("GBP", "USD"), "GBP"), "requires aligned FX")
  fx <- matrix(c(0, 0, 0.01, -0.02), ncol = 2)
  base <- apply_currency_mode(x, "base_currency", c("GBP", "USD"), "GBP", fx)
  expect_equal(base[, 1], x[, 1])
  expect_equal(base[, 2], (1 + x[, 2]) * (1 + fx[, 2]) - 1)
})
