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
  if (anyNA(out$date) || anyNA(out$ticker) || any(!nzchar(out$ticker))) {
    stop("Dates and tickers must be valid.", call. = FALSE)
  }
  duplicate_rows <- duplicated(out[c("date", "ticker")])
  duplicate_count <- sum(duplicate_rows)
  out <- out[!duplicate_rows, , drop = FALSE]
  invalid <- !is.finite(out$adjusted) | out$adjusted <= 0
  invalid_count <- sum(invalid)
  out <- out[!invalid, , drop = FALSE]
  if (!nrow(out)) stop("No valid prices remain.", call. = FALSE)
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
#' @param frequency NULL or daily keeps common observed dates. Weekly uses a
#'   shared Friday valuation calendar and each asset's latest quote on or before
#'   the cutoff, without dropping holiday weeks.
#' @param start,end Optional inclusive date bounds. A weekly cutoff after end is
#'   excluded, including an incomplete final calendar week.
#' @param max_stale_days Maximum quote age at a weekly cutoff, from zero to four
#'   calendar days. Quotes from an earlier week are never carried forward.
#' @return Wide data frame with a date column. Its valuation_audit attribute
#'   records valuation dates, source quote dates, and quote ages.
#' @export
align_prices <- function(prices, tickers = unique(prices$ticker), frequency = NULL,
                         start = NULL, end = NULL, max_stale_days = 4L) {
  if (!is.null(frequency)) frequency <- match.arg(frequency, c("daily", "weekly"))
  if (length(max_stale_days) != 1L || !is.numeric(max_stale_days) ||
      !is.finite(max_stale_days) || max_stale_days != floor(max_stale_days) ||
      max_stale_days < 0L || max_stale_days > 4L) {
    stop("max_stale_days must be an integer from zero to four.", call. = FALSE)
  }
  prices$date <- as.Date(prices$date)
  prices <- prices[prices$ticker %in% tickers, , drop = FALSE]
  if (!nrow(prices)) stop("No configured asset prices remain.", call. = FALSE)
  if (is.null(start)) start <- min(prices$date)
  if (is.null(end)) end <- max(prices$date)
  start <- as.Date(start); end <- as.Date(end)
  if (length(start) != 1L || length(end) != 1L || is.na(start) || is.na(end) || start > end) {
    stop("Alignment start/end must be valid ordered dates.", call. = FALSE)
  }
  prices <- prices[prices$date >= start & prices$date <= end, , drop = FALSE]
  missing_tickers <- setdiff(tickers, unique(prices$ticker))
  if (length(missing_tickers)) {
    stop(sprintf("No in-range prices for: %s", paste(missing_tickers, collapse = ", ")), call. = FALSE)
  }
  if (identical(frequency, "weekly")) {
    # Friday is day 5 in POSIXlt's Sunday-based weekday convention.
    next_friday <- function(date) date + (5L - as.POSIXlt(date)$wday) %% 7L
    first_cutoff <- next_friday(max(start, min(prices$date)))
    # Do not invent additional weeks beyond the last week represented in input.
    available_end <- min(end, next_friday(max(prices$date)))
    if (first_cutoff > available_end) stop("No complete weekly valuation period remains.", call. = FALSE)
    cutoffs <- seq(first_cutoff, available_end, by = "7 days")
    out <- data.frame(date = cutoffs)
    audit <- lapply(tickers, function(ticker) {
      x <- prices[prices$ticker == ticker, c("date", "adjusted"), drop = FALSE]
      x <- x[order(x$date), , drop = FALSE]
      quote_index <- findInterval(as.numeric(cutoffs), as.numeric(x$date))
      if (any(quote_index == 0L)) {
        bad <- which(quote_index == 0L)[[1]]
        stop(sprintf("Missing weekly quote for %s at %s; future quotes cannot be used.",
                     ticker, cutoffs[[bad]]), call. = FALSE)
      }
      quote_dates <- x$date[quote_index]
      ages <- as.integer(cutoffs - quote_dates)
      if (any(ages > max_stale_days)) {
        bad <- which(ages > max_stale_days)[[1]]
        stop(sprintf("Stale or missing weekly quote for %s at %s: source date %s is %d days old (maximum %d).",
                     ticker, cutoffs[[bad]], quote_dates[[bad]], ages[[bad]], max_stale_days), call. = FALSE)
      }
      data.frame(valuation_date = cutoffs, ticker = ticker, quote_date = quote_dates,
                 stale_days = ages, price = x$adjusted[quote_index], stringsAsFactors = FALSE)
    })
    for (i in seq_along(tickers)) out[[tickers[[i]]]] <- audit[[i]]$price
    attr(out, "valuation_audit") <- do.call(rbind, audit)
    attr(out, "frequency") <- "weekly"
    return(out)
  }
  frames <- lapply(tickers, function(ticker) {
    x <- prices[prices$ticker == ticker, c("date", "adjusted"), drop = FALSE]
    names(x)[2] <- ticker
    x
  })
  out <- Reduce(function(x, y) merge(x, y, by = "date", all = FALSE, sort = TRUE), frames)
  if (!nrow(out)) stop("No common trading dates remain after alignment.", call. = FALSE)
  attr(out, "valuation_audit") <- do.call(rbind, lapply(tickers, function(ticker) {
    data.frame(valuation_date = out$date, ticker = ticker, quote_date = out$date,
               stale_days = 0L, price = out[[ticker]], stringsAsFactors = FALSE)
  }))
  if (!is.null(frequency)) attr(out, "frequency") <- frequency
  out
}
