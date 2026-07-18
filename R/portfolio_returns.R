#' Validate portfolio weights
#' @param weights Numeric weights.
#' @param n_assets Expected asset count.
#' @param allow_short Whether negative weights are permitted.
#' @param tolerance Sum-to-one tolerance.
#' @return Validated numeric weights.
#' @export
validate_weights <- function(weights, n_assets = length(weights), allow_short = FALSE, tolerance = 1e-8) {
  weights <- as.numeric(weights)
  if (length(weights) != n_assets || any(!is.finite(weights))) stop("Weights must be finite and match the asset count.", call. = FALSE)
  if (!allow_short && any(weights < 0)) stop("Negative weights require `allow_short = TRUE`.", call. = FALSE)
  if (abs(sum(weights) - 1) > tolerance) stop("Portfolio weights must sum to one within tolerance.", call. = FALSE)
  weights
}

#' Apply an explicit currency mode
#' @param asset_simple_returns Matrix of local-currency simple returns.
#' @param mode `local_index_return` or `base_currency`.
#' @param asset_currencies Currency for each asset.
#' @param base_currency Portfolio base currency.
#' @param fx_simple_returns FX return matrix for base currency per unit of asset currency.
#' @return Base-currency or unchanged simple returns with metadata.
#' @export
apply_currency_mode <- function(asset_simple_returns, mode = "local_index_return",
                                asset_currencies, base_currency, fx_simple_returns = NULL) {
  x <- as.matrix(asset_simple_returns)
  if (length(asset_currencies) != ncol(x)) stop("Asset currency count must match assets.", call. = FALSE)
  if (identical(mode, "local_index_return")) {
    return(structure(x, currency_metadata = list(mode = mode, fx_conversion = FALSE,
      note = "Returns are abstract local-index returns; no FX conversion was performed.")))
  }
  if (!identical(mode, "base_currency")) stop("Unknown currency mode.", call. = FALSE)
  if (is.null(fx_simple_returns)) stop("Base-currency mode requires aligned FX simple returns.", call. = FALSE)
  fx <- as.matrix(fx_simple_returns)
  if (!identical(dim(fx), dim(x))) stop("FX and asset return dimensions must match exactly.", call. = FALSE)
  if (any(!is.finite(fx))) stop("FX returns must be finite.", call. = FALSE)
  converted <- x
  for (j in seq_len(ncol(x))) {
    if (asset_currencies[[j]] == base_currency) converted[, j] <- x[, j]
    else converted[, j] <- (1 + x[, j]) * (1 + fx[, j]) - 1
  }
  structure(converted, currency_metadata = list(mode = mode, fx_conversion = TRUE,
    quote_convention = "base-currency units per asset-currency unit"))
}

#' Calculate exact fixed-weight portfolio returns
#' @param asset_simple_returns Matrix of asset simple returns.
#' @param weights Portfolio weights.
#' @param allow_short Whether shorts are allowed.
#' @return Data frame with portfolio simple and exact log returns.
#' @export
calculate_portfolio_returns <- function(asset_simple_returns, weights = NULL, allow_short = FALSE) {
  x <- as.matrix(asset_simple_returns)
  if (is.null(weights)) weights <- rep(1 / ncol(x), ncol(x))
  weights <- validate_weights(weights, ncol(x), allow_short)
  simple <- drop(x %*% weights)
  if (any(simple <= -1)) stop("Portfolio simple return is at or below -100%; log return is undefined.", call. = FALSE)
  data.frame(simple_return = simple, log_return = log1p(simple))
}
