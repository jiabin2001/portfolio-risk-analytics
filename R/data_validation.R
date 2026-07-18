#' Validate long-format prices
#' @param prices Data frame with `date`, `ticker`, and `adjusted`.
#' @param min_observations Minimum observations per ticker.
#' @param max_stale Maximum consecutive unchanged prices before a warning is recorded.
#' @return A list containing cleaned data, diagnostics, and warnings.
#' @export
validate_prices <- function(prices, min_observations = 100L, max_stale = 10L) {
  needed <- c("date", "ticker", "adjusted")
  if (!is.data.frame(prices) || !all(needed %in% names(prices))) {
    stop("Prices must contain date, ticker, and adjusted columns.", call. = FALSE)
  }
  out <- prices[, needed]
  out$date <- as.Date(out$date)
  out$ticker <- as.character(out$ticker)
  out$adjusted <- as.numeric(out$adjusted)
  if (anyNA(out$date) || any(!nzchar(out$ticker))) stop("Dates and tickers must be valid.", call. = FALSE)
  duplicate_rows <- duplicated(out[c("date", "ticker")])
  duplicate_count <- sum(duplicate_rows)
  out <- out[!duplicate_rows, , drop = FALSE]
  invalid <- !is.finite(out$adjusted) | out$adjusted <= 0
  invalid_count <- sum(invalid)
  out <- out[!invalid, , drop = FALSE]
  out <- out[order(out$ticker, out$date), , drop = FALSE]
  counts <- table(out$ticker)
  too_short <- names(counts[counts < min_observations])
  if (length(too_short)) {
    stop(sprintf("Insufficient valid prices for: %s", paste(too_short, collapse = ", ")), call. = FALSE)
  }
  stale_by_ticker <- vapply(split(out$adjusted, out$ticker), function(x) {
    encoded <- rle(diff(x) == 0)
    runs <- encoded$lengths[encoded$values]
    if (length(runs)) max(runs) + 1L else 1L
  }, integer(1))
  warnings <- character()
  if (duplicate_count) warnings <- c(warnings, sprintf("Removed %d duplicate date/ticker rows.", duplicate_count))
  if (invalid_count) warnings <- c(warnings, sprintf("Removed %d missing, non-finite, or non-positive prices.", invalid_count))
  if (any(stale_by_ticker > max_stale)) {
    warnings <- c(warnings, sprintf("Stale-price run exceeded %d observations for: %s", max_stale,
      paste(names(stale_by_ticker)[stale_by_ticker > max_stale], collapse = ", ")))
  }
  list(
    data = out,
    diagnostics = list(duplicate_count = duplicate_count, invalid_count = invalid_count,
                       observations = counts, max_stale_run = stale_by_ticker),
    warnings = warnings
  )
}

#' Align long-format prices to common trading dates
#' @param prices Validated long-format prices.
#' @param tickers Ordered ticker names.
#' @return Wide data frame with a date column.
#' @export
align_prices <- function(prices, tickers = unique(prices$ticker)) {
  frames <- lapply(tickers, function(ticker) {
    x <- prices[prices$ticker == ticker, c("date", "adjusted"), drop = FALSE]
    names(x)[2] <- ticker
    x
  })
  out <- Reduce(function(x, y) merge(x, y, by = "date", all = FALSE, sort = TRUE), frames)
  if (!nrow(out)) stop("No common trading dates remain after alignment.", call. = FALSE)
  out
}
