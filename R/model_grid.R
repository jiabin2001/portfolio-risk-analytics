#' Build a configuration-driven marginal model grid
#' @param model_config Models section of a configuration.
#' @return Data frame with one row per candidate.
#' @export
build_model_grid <- function(model_config) {
  orders <- model_config$garch_orders
  if (is.null(orders) || !length(orders)) stop("At least one GARCH order is required.", call. = FALSE)
  rows <- list()
  k <- 0L
  for (p in model_config$arma_p) for (q in model_config$arma_q) {
    for (order in orders) for (family in model_config$volatility_families) {
      for (distribution in model_config$distributions) {
        order <- as.integer(unlist(order))
        if (length(order) != 2L || any(order < 0)) stop("Each GARCH order must contain two non-negative integers.", call. = FALSE)
        k <- k + 1L
        rows[[k]] <- data.frame(
          model_id = sprintf("arma%d%d_%s%d%d_%s", p, q, tolower(family), order[1], order[2], distribution),
          arma_p = as.integer(p), arma_q = as.integer(q),
          garch_p = order[1], garch_q = order[2],
          volatility_family = as.character(family), distribution = as.character(distribution),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  do.call(rbind, rows)
}
