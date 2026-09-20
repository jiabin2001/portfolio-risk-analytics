#' Calculate asset simple and log returns
#' @param aligned_prices Wide prices with a `date` column.
#' @return List with aligned dates, simple returns, and log returns.
#' @export
calculate_asset_returns <- function(aligned_prices) {
  if (!is.data.frame(aligned_prices) || !"date" %in% names(aligned_prices) || ncol(aligned_prices) < 3L) {
    stop("Aligned prices require date and at least two asset columns.", call. = FALSE)
  }
  dates <- as.Date(aligned_prices$date)
  if (length(dates) < 2L || anyNA(dates) || any(diff(dates) <= 0)) {
    stop("Prices require at least two strictly increasing valuation dates.", call. = FALSE)
  }
  if (identical(attr(aligned_prices, "frequency"), "weekly") && any(as.integer(diff(dates)) != 7L)) {
    stop("Weekly valuation dates must be exactly seven days apart; missing weeks cannot be collapsed.", call. = FALSE)
  }
  values <- as.matrix(aligned_prices[-1])
  storage.mode(values) <- "double"
  if (any(!is.finite(values)) || any(values <= 0)) stop("Prices must be finite and positive.", call. = FALSE)
  simple <- values[-1, , drop = FALSE] / values[-nrow(values), , drop = FALSE] - 1
  log_returns <- log(values[-1, , drop = FALSE] / values[-nrow(values), , drop = FALSE])
  colnames(simple) <- colnames(values)
  colnames(log_returns) <- colnames(values)
  list(dates = as.Date(aligned_prices$date[-1]), simple = simple, log = log_returns)
}
