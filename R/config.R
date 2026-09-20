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
  require_fields <- function(section, fields, name) {
    missing_fields <- fields[vapply(fields, function(field) is.null(section[[field]]), logical(1))]
    if (length(missing_fields)) {
      stop(sprintf("Missing `%s` field(s): %s", name, paste(missing_fields, collapse = ", ")), call. = FALSE)
    }
  }
  require_flag <- function(value, name) {
    if (length(value) != 1L || !is.logical(value) || is.na(value)) {
      stop(sprintf("`%s` must be TRUE or FALSE.", name), call. = FALSE)
    }
  }
  require_integer <- function(value, name, minimum = 0L, allow_vector = FALSE) {
    ok_length <- if (allow_vector) length(value) >= 1L else length(value) == 1L
    if (!ok_length || !is.numeric(value) || any(!is.finite(value)) ||
        any(value != floor(value)) || any(value < minimum)) {
      qualifier <- if (allow_vector) "one or more" else "one"
      stop(sprintf("`%s` must contain %s integer value(s) not below %d.", name, qualifier, minimum), call. = FALSE)
    }
  }
  require_choice <- function(value, choices, name) {
    if (length(value) != 1L || !is.character(value) || !value %in% choices) {
      stop(sprintf("`%s` must be one of: %s.", name, paste(choices, collapse = ", ")), call. = FALSE)
    }
  }

  required <- c("profile", "data", "portfolio", "risk", "models", "simulation", "rolling", "compute", "report")
  missing <- setdiff(required, names(cfg))
  if (length(missing)) stop(sprintf("Missing configuration section(s): %s", paste(missing, collapse = ", ")), call. = FALSE)
  assert_scalar(cfg$profile, "profile", "character")

  require_fields(cfg$data, c("source", "tickers", "start", "end", "frequency", "adjusted", "cache",
                             "force_refresh", "min_observations"), "data")
  require_choice(cfg$data$source, c("fixture", "csv", "yahoo"), "data.source")
  if (length(cfg$data$tickers) < 2L) stop("At least two tickers are required.", call. = FALSE)
  if (!is.character(cfg$data$tickers) || any(!nzchar(cfg$data$tickers))) stop("Tickers must be non-empty strings.", call. = FALSE)
  start <- as.Date(cfg$data$start); end <- as.Date(cfg$data$end)
  if (is.na(start) || is.na(end) || start > end) stop("Data start/end must be valid ordered dates.", call. = FALSE)
  require_choice(cfg$data$frequency, c("daily", "weekly"), "data.frequency")
  require_flag(cfg$data$adjusted, "data.adjusted")
  require_flag(cfg$data$cache, "data.cache")
  require_flag(cfg$data$force_refresh, "data.force_refresh")
  require_integer(cfg$data$min_observations, "data.min_observations", 30L)
  if (!is.null(cfg$data$max_stale_days)) {
    require_integer(cfg$data$max_stale_days, "data.max_stale_days")
    if (cfg$data$max_stale_days > 4L) stop("Weekly prices must come from the same trading week (max_stale_days <= 4).", call. = FALSE)
  }
  if (anyDuplicated(cfg$data$tickers)) stop("Tickers must be unique.", call. = FALSE)
  if (cfg$data$source == "fixture") {
    require_fields(cfg$data, c("fixture_observations", "fixture_seed"), "data")
    require_integer(cfg$data$fixture_observations, "data.fixture_observations", cfg$data$min_observations)
    require_integer(cfg$data$fixture_seed, "data.fixture_seed")
  }
  if (cfg$data$source == "csv") {
    assert_scalar(cfg$data$path, "data.path", "character")
  }

  require_fields(cfg$portfolio, c("weights", "return_type", "currency_mode", "asset_currencies",
                                  "base_currency", "allow_short"), "portfolio")
  require_flag(cfg$portfolio$allow_short, "portfolio.allow_short")
  validate_weights(cfg$portfolio$weights, length(cfg$data$tickers), cfg$portfolio$allow_short %||% FALSE)
  require_choice(cfg$portfolio$return_type, c("log", "simple"), "portfolio.return_type")
  require_choice(cfg$portfolio$currency_mode, c("local_index_return", "base_currency"), "portfolio.currency_mode")
  if (length(cfg$portfolio$asset_currencies) != length(cfg$data$tickers)) {
    stop("Each asset requires an explicit currency.", call. = FALSE)
  }
  if (!is.character(cfg$portfolio$asset_currencies) || any(!nzchar(cfg$portfolio$asset_currencies))) {
    stop("Asset currencies must be non-empty strings.", call. = FALSE)
  }
  assert_scalar(cfg$portfolio$base_currency, "portfolio.base_currency", "character")

  require_fields(cfg$risk, "confidence_levels", "risk")
  if (!is.numeric(cfg$risk$confidence_levels) || !length(cfg$risk$confidence_levels)) {
    stop("`risk.confidence_levels` must be a non-empty numeric vector.", call. = FALSE)
  }
  vapply(cfg$risk$confidence_levels, validate_confidence, numeric(1))

  require_fields(cfg$models, c("arma_p", "arma_q", "garch_orders", "volatility_families", "distributions",
                               "information_criterion", "diagnostic_alpha", "solver", "timeout_seconds",
                               "reselection_frequency", "copula_families"), "models")
  require_integer(cfg$models$arma_p, "models.arma_p", allow_vector = TRUE)
  require_integer(cfg$models$arma_q, "models.arma_q", allow_vector = TRUE)
  if (!is.list(cfg$models$garch_orders) || !length(cfg$models$garch_orders)) {
    stop("`models.garch_orders` must contain at least one two-integer order.", call. = FALSE)
  }
  for (order in cfg$models$garch_orders) {
    order <- unlist(order)
    require_integer(order, "models.garch_orders", allow_vector = TRUE)
    if (length(order) != 2L) stop("Each GARCH order must contain exactly two integers.", call. = FALSE)
  }
  valid_volatility <- c("sGARCH", "eGARCH", "gjrGARCH")
  if (!length(cfg$models$volatility_families) || any(!cfg$models$volatility_families %in% valid_volatility)) {
    stop("Unsupported volatility family in `models.volatility_families`.", call. = FALSE)
  }
  valid_distributions <- c("norm", "std", "sstd", "ged", "sged")
  if (!length(cfg$models$distributions) || any(!cfg$models$distributions %in% valid_distributions)) {
    stop("Unsupported innovation distribution in `models.distributions`.", call. = FALSE)
  }
  require_choice(toupper(cfg$models$information_criterion), c("AIC", "BIC"), "models.information_criterion")
  assert_scalar(cfg$models$diagnostic_alpha, "models.diagnostic_alpha", "numeric")
  if (cfg$models$diagnostic_alpha <= 0 || cfg$models$diagnostic_alpha >= 1) {
    stop("`models.diagnostic_alpha` must be strictly between zero and one.", call. = FALSE)
  }
  assert_scalar(cfg$models$solver, "models.solver", "character")
  require_integer(cfg$models$timeout_seconds, "models.timeout_seconds", 1L)
  require_integer(cfg$models$reselection_frequency, "models.reselection_frequency", 1L)
  valid_copulas <- c("gaussian", "student", "clayton", "gumbel", "frank", "bb1",
                     "survival_clayton", "survival_gumbel", "clayton_90", "gumbel_90",
                     "clayton_270", "gumbel_270", "survival_bb1", "bb1_90", "bb1_270")
  if (!length(cfg$models$copula_families) || any(!cfg$models$copula_families %in% valid_copulas)) {
    stop("Unsupported copula family in `models.copula_families`.", call. = FALSE)
  }

  require_fields(cfg$simulation, c("final_n", "rolling_n", "chunk_size", "seed", "keep_asset_returns"), "simulation")
  require_integer(cfg$simulation$final_n, "simulation.final_n", 1000L)
  require_integer(cfg$simulation$rolling_n, "simulation.rolling_n", 1000L)
  require_integer(cfg$simulation$chunk_size, "simulation.chunk_size", 1L)
  require_integer(cfg$simulation$seed, "simulation.seed")
  require_flag(cfg$simulation$keep_asset_returns, "simulation.keep_asset_returns")
  convergence_present <- !is.null(cfg$simulation$convergence_counts) || !is.null(cfg$simulation$convergence_seeds)
  if (convergence_present) {
    require_fields(cfg$simulation, c("convergence_counts", "convergence_seeds"), "simulation")
    require_integer(cfg$simulation$convergence_counts, "simulation.convergence_counts", 1000L, allow_vector = TRUE)
    require_integer(cfg$simulation$convergence_seeds, "simulation.convergence_seeds", allow_vector = TRUE)
  }

  require_fields(cfg$rolling, c("window_type", "initial_window", "window_size", "evaluation_observations",
                                "models", "checkpoint"), "rolling")
  require_choice(cfg$rolling$window_type, c("moving", "expanding"), "rolling.window_type")
  require_integer(cfg$rolling$initial_window, "rolling.initial_window", 50L)
  require_integer(cfg$rolling$window_size, "rolling.window_size", 30L)
  require_integer(cfg$rolling$evaluation_observations, "rolling.evaluation_observations", 1L)
  valid_rolling <- c("historical", "gaussian", "student_t", "ewma", "filtered_historical", "garch_t", "copula_garch")
  if (!length(cfg$rolling$models) || any(!cfg$rolling$models %in% valid_rolling)) {
    stop("Unsupported model in `rolling.models`.", call. = FALSE)
  }
  require_flag(cfg$rolling$checkpoint, "rolling.checkpoint")
  if (!is.null(cfg$rolling$copula_ablations)) {
    if (any(!cfg$rolling$copula_ablations %in% c("independence", "gaussian", "student", "bb1")) ||
        anyDuplicated(cfg$rolling$copula_ablations)) stop("Invalid or duplicated rolling.copula_ablations.", call. = FALSE)
    if (length(cfg$rolling$copula_ablations) && !"copula_garch" %in% cfg$rolling$models) {
      stop("Copula ablations require rolling.models to include copula_garch.", call. = FALSE)
    }
  }
  if ("garch_t" %in% cfg$rolling$models && min(cfg$rolling$initial_window, cfg$rolling$window_size) < 100L) {
    stop("garch_t requires at least 100 training observations.", call. = FALSE)
  }
  if (!is.null(cfg$evaluation)) {
    require_fields(cfg$evaluation, c("reference_model", "bootstrap_replications", "block_length", "seed"), "evaluation")
    require_choice(cfg$evaluation$reference_model, cfg$rolling$models, "evaluation.reference_model")
    require_integer(cfg$evaluation$bootstrap_replications, "evaluation.bootstrap_replications", 100L)
    require_integer(cfg$evaluation$block_length, "evaluation.block_length", 1L)
    require_integer(cfg$evaluation$seed, "evaluation.seed")
  }

  require_fields(cfg$compute, "resume", "compute")
  require_flag(cfg$compute$resume, "compute.resume")
  require_fields(cfg$report, "render", "report")
  require_flag(cfg$report$render, "report.render")
  cfg
}
