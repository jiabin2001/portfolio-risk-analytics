if (!"package:portfoliorisk" %in% search()) {
  bootstrap <- c(file.path("scripts", "bootstrap.R"), file.path("..", "..", "scripts", "bootstrap.R"))
  bootstrap <- bootstrap[file.exists(bootstrap)][1]
  if (is.na(bootstrap)) stop("Could not locate scripts/bootstrap.R")
  source(bootstrap)
}

make_test_config <- function() list(
  profile = "test",
  data = list(
    source = "fixture", tickers = c("ASSET_A", "ASSET_B"),
    start = "2018-01-05", end = "2024-01-01", frequency = "weekly",
    adjusted = TRUE, cache = TRUE, force_refresh = FALSE,
    min_observations = 150, fixture_observations = 220, fixture_seed = 1107
  ),
  portfolio = list(
    weights = c(0.5, 0.5), return_type = "log", currency_mode = "local_index_return",
    asset_currencies = c("GBP", "USD"), base_currency = "GBP", allow_short = FALSE
  ),
  risk = list(confidence_levels = c(0.95, 0.99)),
  models = list(
    arma_p = 0, arma_q = 0, garch_orders = list(c(1, 1)),
    volatility_families = "sGARCH", distributions = "norm", information_criterion = "BIC",
    diagnostic_alpha = 0.01, solver = "hybrid", timeout_seconds = 30,
    reselection_frequency = 20, copula_families = "gaussian"
  ),
  simulation = list(final_n = 1000, rolling_n = 1000, chunk_size = 1000,
                    seed = 20260718, keep_asset_returns = FALSE),
  rolling = list(window_type = "moving", initial_window = 180,
                 window_size = 180, evaluation_observations = 5,
                 models = c("historical", "gaussian"), checkpoint = TRUE),
  compute = list(resume = TRUE),
  report = list(render = FALSE)
)
