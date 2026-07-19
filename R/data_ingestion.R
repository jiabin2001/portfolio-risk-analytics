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
  shocks <- with_preserved_seed(seed, {
    correlation <- 0.55
    common <- rnorm(n)
    vapply(seq_along(tickers), function(i) {
      z <- correlation * common + sqrt(1 - correlation^2) * rnorm(n)
      scale <- 0.012 + 0.003 * (i - 1)
      scale * z * sqrt(0.75 + 0.25 * common^2)
    }, numeric(n))
  })
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

#' Recognise absolute local and UNC data paths
#' @noRd
is_absolute_data_path <- function(path) {
  is.character(path) && length(path) == 1L && !is.na(path) &&
    grepl("^(?:[A-Za-z]:[\\\\/]|/|\\\\\\\\)", path, perl = TRUE)
}

#' Resolve a configured data path without rewriting absolute paths
#' @noRd
resolve_data_path <- function(path, root) {
  if (is_absolute_data_path(path)) path else file.path(root, path)
}

#' Build a complete data-request signature
#' @keywords internal
data_request_signature <- function(cfg, root) {
  csv_path <- cfg$data$path %||% NA_character_
  if (identical(cfg$data$source, "csv") && !is.na(csv_path)) {
    csv_path <- resolve_data_path(csv_path, root)
    csv_path <- normalizePath(csv_path, winslash = "/", mustWork = FALSE)
  }
  list(
    source = cfg$data$source,
    tickers = as.character(cfg$data$tickers),
    start = as.character(as.Date(cfg$data$start)),
    end = as.character(as.Date(cfg$data$end)),
    frequency = cfg$data$frequency,
    adjusted = isTRUE(cfg$data$adjusted),
    csv_path = csv_path,
    csv_md5 = if (!is.na(csv_path) && file.exists(csv_path)) unname(tools::md5sum(csv_path)) else NA_character_
  )
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
  request_signature <- data_request_signature(cfg, root)
  key <- paste(gsub("[^A-Za-z0-9]", "_", cfg$profile), substr(stable_object_md5(request_signature), 1L, 16L), sep = "_")
  cache_path <- file.path(dirs[["cache"]], paste0("prices_", key, ".rds"))
  raw_path <- file.path(dirs[["raw"]], paste0("prices_raw_", key, ".rds"))
  processed_path <- file.path(dirs[["processed"]], paste0("prices_aligned_", key, ".rds"))
  use_cached <- function(cached, warning = NULL) {
    if (!is.list(cached) || !is.data.frame(cached$prices) || !is.data.frame(cached$aligned) ||
        !identical(cached$metadata$request_signature, request_signature)) {
      stop(
        "Cached price data do not match the complete configured request; rerun with `data.force_refresh: true` or `force_refresh = TRUE`.",
        call. = FALSE
      )
    }
    if (!file.exists(raw_path)) atomic_save_rds(cached$prices, raw_path)
    if (!file.exists(processed_path)) atomic_save_rds(cached$aligned, processed_path)
    cached$metadata$cache_hit <- TRUE
    cached$metadata$raw_artifact <- file.path("data", "raw", basename(raw_path))
    cached$metadata$processed_artifact <- file.path("data", "processed", basename(processed_path))
    if (!is.null(warning)) cached$metadata$warnings <- unique(c(cached$metadata$warnings, warning))
    cached
  }
  if (!force_refresh && file.exists(cache_path)) {
    return(use_cached(readRDS(cache_path)))
  }
  source <- cfg$data$source
  if (identical(source, "fixture")) {
    prices <- generate_fixture_prices(cfg$data$tickers, cfg$data$fixture_observations,
                                      as.Date(cfg$data$start), cfg$data$fixture_seed)
    metadata <- attr(prices, "metadata")
  } else if (identical(source, "csv")) {
    if (is.null(cfg$data$path)) stop("CSV source requires `data.path`.", call. = FALSE)
    input_path <- resolve_data_path(cfg$data$path, root)
    prices <- read.csv(input_path, stringsAsFactors = FALSE)
    metadata <- list(source = "csv", input = cfg$data$path)
  } else if (identical(source, "yahoo")) {
    if (!requireNamespace("quantmod", quietly = TRUE)) {
      if (file.exists(cache_path)) {
        return(use_cached(readRDS(cache_path), "Refresh skipped because quantmod is unavailable; used matching cache."))
      }
      stop("Online Yahoo acquisition requires the optional `quantmod` package and no cache is available.", call. = FALSE)
    }
    downloads <- tryCatch(lapply(cfg$data$tickers, function(ticker) {
        x <- quantmod::getSymbols(ticker, src = "yahoo", from = cfg$data$start,
                                 to = cfg$data$end, auto.assign = FALSE, warnings = FALSE)
        if (identical(cfg$data$frequency, "weekly")) x <- xts::to.weekly(x, drop.time = TRUE)
        field <- if (isTRUE(cfg$data$adjusted)) "adjusted" else "close"
        values <- if (field == "adjusted") {
          tryCatch(quantmod::Ad(x), error = function(e) {
            stop(sprintf("Adjusted Yahoo prices are unavailable for %s: %s", ticker, conditionMessage(e)), call. = FALSE)
          })
        } else {
          quantmod::Cl(x)
        }
        list(
          data = data.frame(date = as.Date(zoo::index(values)), ticker = ticker,
                            adjusted = as.numeric(values), stringsAsFactors = FALSE),
          price_field = field
        )
      }), error = function(e) e)
    if (inherits(downloads, "error")) {
      if (file.exists(cache_path)) {
        return(use_cached(readRDS(cache_path), sprintf("Refresh failed; used matching cache: %s", conditionMessage(downloads))))
      }
      stop(conditionMessage(downloads), call. = FALSE)
    }
    prices <- do.call(rbind, lapply(downloads, `[[`, "data"))
    metadata <- list(source = "yahoo", requested_start = cfg$data$start,
                      requested_end = cfg$data$end,
                      price_fields = setNames(vapply(downloads, `[[`, character(1), "price_field"), cfg$data$tickers))
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
    request_signature = request_signature,
    raw_artifact = file.path("data", "raw", basename(raw_path)),
    processed_artifact = file.path("data", "processed", basename(processed_path))
  ))
  result <- list(prices = checked$data, aligned = aligned, metadata = metadata)
  atomic_save_rds(checked$data, raw_path)
  atomic_save_rds(aligned, processed_path)
  if (isTRUE(cfg$data$cache)) atomic_save_rds(result, cache_path)
  result
}
