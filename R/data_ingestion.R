#' Generate deterministic, explicitly synthetic fixture prices
#'
#' This function is for software tests and smoke runs only. It does not represent real
#' market data.
#' @param tickers Asset labels.
#' @param n Number of weekly observations.
#' @param start First date.
#' @param seed Random seed.
#' @return Long-format price data with fixture metadata.
#' @export
generate_fixture_prices <- function(tickers = c("ASSET_A", "ASSET_B"), n = 320L,
                                    start = as.Date("2018-01-05"), seed = 1107L) {
  if (length(tickers) < 2L || n < 100L) stop("Fixture requires at least two assets and 100 observations.", call. = FALSE)
  set.seed(seed)
  correlation <- 0.55
  common <- rnorm(n)
  shocks <- vapply(seq_along(tickers), function(i) {
    z <- correlation * common + sqrt(1 - correlation^2) * rnorm(n)
    scale <- 0.012 + 0.003 * (i - 1)
    scale * z * sqrt(0.75 + 0.25 * common^2)
  }, numeric(n))
  dates <- start + 7L * (seq_len(n) - 1L)
  pieces <- lapply(seq_along(tickers), function(i) data.frame(
    date = dates, ticker = tickers[[i]],
    adjusted = 100 * exp(cumsum(shocks[, i])), stringsAsFactors = FALSE
  ))
  out <- do.call(rbind, pieces)
  attr(out, "metadata") <- list(source = "synthetic_fixture", seed = seed,
                                 generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE))
  out
}

#' Load price data through the configured cache
#' @param cfg Validated configuration.
#' @param root Project root.
#' @param force_refresh Override the configuration refresh flag.
#' @return List containing prices and acquisition metadata.
#' @export
load_price_data <- function(cfg, root = pra_project_root(), force_refresh = NULL) {
  dirs <- ensure_project_dirs(root)
  force_refresh <- force_refresh %||% (cfg$data$force_refresh %||% FALSE)
  key <- paste(cfg$profile, paste(gsub("[^A-Za-z0-9]", "_", cfg$data$tickers), collapse = "_"), sep = "_")
  cache_path <- file.path(dirs[["cache"]], paste0("prices_", key, ".rds"))
  raw_path <- file.path(dirs[["raw"]], paste0("prices_raw_", key, ".rds"))
  processed_path <- file.path(dirs[["processed"]], paste0("prices_aligned_", key, ".rds"))
  if (!force_refresh && file.exists(cache_path)) {
    cached <- readRDS(cache_path)
    if (!file.exists(raw_path)) atomic_save_rds(cached$prices, raw_path)
    if (!file.exists(processed_path)) atomic_save_rds(cached$aligned, processed_path)
    cached$metadata$cache_hit <- TRUE
    cached$metadata$raw_artifact <- file.path("data", "raw", basename(raw_path))
    cached$metadata$processed_artifact <- file.path("data", "processed", basename(processed_path))
    return(cached)
  }
  source <- cfg$data$source
  if (identical(source, "fixture")) {
    prices <- generate_fixture_prices(cfg$data$tickers, cfg$data$fixture_observations,
                                      as.Date(cfg$data$start), cfg$data$fixture_seed)
    metadata <- attr(prices, "metadata")
  } else if (identical(source, "csv")) {
    if (is.null(cfg$data$path)) stop("CSV source requires `data.path`.", call. = FALSE)
    input_path <- if (grepl("^[A-Za-z]:|^/", cfg$data$path)) cfg$data$path else file.path(root, cfg$data$path)
    prices <- read.csv(input_path, stringsAsFactors = FALSE)
    metadata <- list(source = "csv", input = cfg$data$path)
  } else if (identical(source, "yahoo")) {
    if (!requireNamespace("quantmod", quietly = TRUE)) {
      if (file.exists(cache_path)) return(readRDS(cache_path))
      stop("Online Yahoo acquisition requires the optional `quantmod` package and no cache is available.", call. = FALSE)
    }
    pieces <- tryCatch(lapply(cfg$data$tickers, function(ticker) {
        x <- quantmod::getSymbols(ticker, src = "yahoo", from = cfg$data$start,
                                 to = cfg$data$end, auto.assign = FALSE, warnings = FALSE)
        if (identical(cfg$data$frequency, "weekly")) x <- xts::to.weekly(x, drop.time = TRUE)
        adjusted <- tryCatch(quantmod::Ad(x), error = function(e) quantmod::Cl(x))
        data.frame(date = as.Date(zoo::index(adjusted)), ticker = ticker,
                   adjusted = as.numeric(adjusted), stringsAsFactors = FALSE)
      }), error = function(e) e)
    if (inherits(pieces, "error")) {
      if (file.exists(cache_path)) {
        cached <- readRDS(cache_path)
        cached$metadata$cache_hit <- TRUE
        cached$metadata$warnings <- unique(c(cached$metadata$warnings,
          sprintf("Refresh failed; used cache: %s", conditionMessage(pieces))))
        return(cached)
      }
      stop(conditionMessage(pieces), call. = FALSE)
    }
    prices <- do.call(rbind, pieces)
    metadata <- list(source = "yahoo", requested_start = cfg$data$start,
                     requested_end = cfg$data$end)
  } else {
    stop(sprintf("Unsupported data source: %s", source), call. = FALSE)
  }
  checked <- validate_prices(prices, cfg$data$min_observations)
  aligned <- align_prices(checked$data, cfg$data$tickers)
  date_range <- range(aligned$date)
  requested_start <- as.Date(cfg$data$start); requested_end <- as.Date(cfg$data$end)
  if (date_range[2] < requested_start || date_range[1] > requested_end) {
    stop("Downloaded data do not overlap the requested date range.", call. = FALSE)
  }
  metadata <- c(metadata, list(
    downloaded_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    cache_hit = FALSE, date_min = as.character(date_range[1]),
    date_max = as.character(date_range[2]), warnings = checked$warnings,
    raw_artifact = file.path("data", "raw", basename(raw_path)),
    processed_artifact = file.path("data", "processed", basename(processed_path))
  ))
  result <- list(prices = checked$data, aligned = aligned, metadata = metadata)
  atomic_save_rds(checked$data, raw_path)
  atomic_save_rds(aligned, processed_path)
  if (isTRUE(cfg$data$cache)) atomic_save_rds(result, cache_path)
  result
}
