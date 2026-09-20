make_calendar_prices <- function() {
  data.frame(
    date = as.Date(c("2025-06-27", "2025-07-04", "2025-07-11",
                     "2025-06-27", "2025-07-03", "2025-07-11")),
    ticker = rep(c("UK", "US"), each = 3L), adjusted = c(100, 101, 102, 100, 99, 98)
  )
}

test_that("shared Friday valuations retain a holiday week with source quote audit", {
  prices <- make_calendar_prices()
  aligned <- align_prices(prices, c("UK", "US"), frequency = "weekly",
                          start = "2025-06-27", end = "2025-07-11")
  expect_equal(aligned$date, as.Date(c("2025-06-27", "2025-07-04", "2025-07-11")))
  expect_equal(aligned$US, c(100, 99, 98))
  expect_equal(as.integer(diff(aligned$date)), c(7L, 7L))
  audit <- attr(aligned, "valuation_audit")
  holiday <- audit[audit$ticker == "US" & audit$valuation_date == as.Date("2025-07-04"), ]
  expect_equal(holiday$quote_date, as.Date("2025-07-03"))
  expect_equal(holiday$stale_days, 1L)
  returns <- calculate_asset_returns(aligned)
  expect_equal(unname(returns$simple[, "US"]), c(-0.01, 98 / 99 - 1))
})

test_that("weekly alignment never uses future prices or collapses a missing week", {
  prices <- make_calendar_prices()
  prices$adjusted[prices$ticker == "US" & prices$date == as.Date("2025-07-11")] <- 999
  aligned <- align_prices(prices, c("UK", "US"), "weekly", end = "2025-07-11")
  expect_equal(aligned$US[[2]], 99)
  expect_true(all(attr(aligned, "valuation_audit")$quote_date <=
                    attr(aligned, "valuation_audit")$valuation_date))
  missing_week <- prices[!(prices$ticker == "US" & prices$date == as.Date("2025-07-03")), ]
  expect_error(align_prices(missing_week, c("UK", "US"), "weekly"), "Stale or missing weekly quote")
  future_only <- prices[!(prices$ticker == "US" & prices$date == as.Date("2025-06-27")), ]
  expect_error(align_prices(future_only, c("UK", "US"), "weekly"), "future quotes cannot be used")
  expect_error(align_prices(prices, c("UK", "US"), "weekly", max_stale_days = 0),
               "Stale or missing weekly quote")
  collapsed <- aligned[c(1, 3), ]
  attr(collapsed, "frequency") <- "weekly"
  expect_error(calculate_asset_returns(collapsed), "missing weeks cannot be collapsed")
})

test_that("incomplete trailing weeks are excluded and shared market holidays are retained", {
  prices <- make_calendar_prices()
  extra <- data.frame(date = as.Date("2025-07-15"), ticker = c("UK", "US"), adjusted = c(103, 97))
  aligned <- align_prices(rbind(prices, extra), c("UK", "US"), "weekly", end = "2025-07-15")
  expect_equal(max(aligned$date), as.Date("2025-07-11"))
  both_closed <- prices
  both_closed$date[both_closed$date == as.Date("2025-07-11")] <- as.Date("2025-07-10")
  aligned_holiday <- align_prices(both_closed, c("UK", "US"), "weekly", end = "2025-07-11")
  expect_equal(max(aligned_holiday$date), as.Date("2025-07-11"))
  expect_equal(tail(aligned_holiday$US, 1), 98)
})

test_that("CSV bounds and requested frequency apply before return calculations", {
  root <- tempfile("pra-csv-calendar-")
  dir.create(root)
  cfg <- make_test_config()
  dates <- seq(as.Date("2025-06-20"), as.Date("2025-07-18"), by = "day")
  dates <- dates[as.POSIXlt(dates)$wday %in% 1:5]
  prices <- data.frame(date = rep(dates, 2), ticker = rep(c("UK", "US"), each = length(dates)),
                       adjusted = c(100 + seq_along(dates), 200 + seq_along(dates)))
  prices <- prices[!(prices$ticker == "US" & prices$date == as.Date("2025-07-04")), ]
  input <- file.path(root, "prices.csv")
  write.csv(prices, input, row.names = FALSE)
  cfg$data$source <- "csv"
  cfg$data$path <- input
  cfg$data$tickers <- c("UK", "US")
  cfg$data$start <- "2025-06-27"
  cfg$data$end <- "2025-07-15"
  cfg$data$min_observations <- 3L
  cfg$data$cache <- FALSE
  weekly <- load_price_data(cfg, root)
  expect_equal(weekly$aligned$date, as.Date(c("2025-06-27", "2025-07-04", "2025-07-11")))
  expect_true(all(weekly$prices$date >= as.Date(cfg$data$start)))
  expect_true(all(weekly$prices$date <= as.Date(cfg$data$end)))
  expect_equal(weekly$metadata$calendar$stale_quote_count, 1L)
  expect_equal(weekly$valuation_audit, attr(weekly$aligned, "valuation_audit"))
  cfg$data$frequency <- "daily"
  daily <- load_price_data(cfg, root)
  expect_gt(nrow(daily$aligned), nrow(weekly$aligned))
  expect_equal(max(daily$aligned$date), as.Date("2025-07-15"))
  expect_false(as.Date("2025-07-04") %in% daily$aligned$date)
  cfg$data$frequency <- "weekly"
  cfg$data$min_observations <- 4L
  expect_error(load_price_data(cfg, root), "Only 3 aligned weekly observations")
})

test_that("fixture generation uses Fridays and cache identity includes all fixture controls", {
  fixture <- generate_fixture_prices(n = 120, start = as.Date("2020-01-01"))
  expect_true(all(as.POSIXlt(fixture$date)$wday == 5L))
  expect_equal(min(fixture$date), as.Date("2020-01-03"))
  root <- tempfile("pra-fixture-cache-")
  dir.create(root)
  cfg <- make_test_config()
  original <- load_price_data(cfg, root)
  expect_false(original$metadata$cache_hit)
  expect_true(load_price_data(cfg, root)$metadata$cache_hit)
  new_seed <- cfg
  new_seed$data$fixture_seed <- cfg$data$fixture_seed + 1L
  changed_seed <- load_price_data(new_seed, root)
  expect_false(changed_seed$metadata$cache_hit)
  expect_false(identical(changed_seed$aligned$ASSET_A, original$aligned$ASSET_A))
  new_count <- cfg
  new_count$data$fixture_observations <- cfg$data$fixture_observations + 1L
  changed_count <- load_price_data(new_count, root)
  expect_false(changed_count$metadata$cache_hit)
  expect_equal(nrow(changed_count$aligned), nrow(original$aligned) + 1L)
  stricter <- cfg
  stricter$data$min_observations <- 221L
  expect_error(load_price_data(stricter, root), "Insufficient valid prices")
  expect_identical(original$metadata$request_signature$schema, 2L)
})

test_that("cache false bypasses an existing cache and leaves it untouched", {
  root <- tempfile("pra-cache-disabled-")
  dir.create(root)
  cfg <- make_test_config()
  original <- load_price_data(cfg, root)
  cache_file <- list.files(file.path(root, "data", "cache"), full.names = TRUE)[[1]]
  poisoned <- readRDS(cache_file)
  poisoned$aligned$ASSET_A[[1]] <- 99999
  saveRDS(poisoned, cache_file)
  before <- unname(tools::md5sum(cache_file))
  cfg$data$cache <- FALSE
  fresh <- load_price_data(cfg, root)
  expect_false(fresh$metadata$cache_hit)
  expect_equal(fresh$aligned$ASSET_A, original$aligned$ASSET_A)
  expect_identical(unname(tools::md5sum(cache_file)), before)
})
