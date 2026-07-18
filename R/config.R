#' Read and validate an analysis configuration
#' @param path YAML configuration path.
#' @return Validated configuration list, including its source path.
#' @export
read_config <- function(path) {
  assert_scalar(path, "path", "character")
  if (!file.exists(path)) stop(sprintf("Configuration does not exist: %s", path), call. = FALSE)
  cfg <- yaml::read_yaml(path)
  cfg$config_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  validate_config(cfg)
}

#' Validate analysis configuration
#' @param cfg Configuration list.
#' @return Configuration, invisibly.
#' @export
validate_config <- function(cfg) {
  required <- c("profile", "data", "portfolio", "risk", "models", "simulation", "rolling", "compute")
  missing <- setdiff(required, names(cfg))
  if (length(missing)) stop(sprintf("Missing configuration section(s): %s", paste(missing, collapse = ", ")), call. = FALSE)
  if (length(cfg$data$tickers) < 2L) stop("At least two tickers are required.", call. = FALSE)
  if (!cfg$data$frequency %in% c("daily", "weekly")) stop("Frequency must be daily or weekly.", call. = FALSE)
  validate_weights(cfg$portfolio$weights, length(cfg$data$tickers), cfg$portfolio$allow_short %||% FALSE)
  if (!cfg$portfolio$currency_mode %in% c("local_index_return", "base_currency")) {
    stop("Currency mode must be local_index_return or base_currency.", call. = FALSE)
  }
  if (length(cfg$portfolio$asset_currencies) != length(cfg$data$tickers)) {
    stop("Each asset requires an explicit currency.", call. = FALSE)
  }
  vapply(cfg$risk$confidence_levels, validate_confidence, numeric(1))
  if (cfg$simulation$final_n < 1000L) stop("Final simulation count must be at least 1,000.", call. = FALSE)
  if (cfg$rolling$initial_window < 50L) stop("Initial rolling window is too short.", call. = FALSE)
  cfg
}
