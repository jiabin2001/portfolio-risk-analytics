#' Open a project PNG device and evaluate plotting code
#' @keywords internal
write_png <- function(path, code, width = 1400, height = 850, res = 140) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  grDevices::png(path, width = width, height = height, res = res)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(code)
  invisible(path)
}

#' Plot aligned asset prices
#' @param aligned_prices Wide price data.
#' @param path Output PNG path.
#' @export
plot_asset_prices <- function(aligned_prices, path) {
  write_png(path, {
    values <- as.matrix(aligned_prices[-1])
    matplot(aligned_prices$date, values, type = "l", lty = 1, lwd = 1.5,
            col = c("#1B4965", "#CA6702", "#4F772D", "#6D597A")[seq_len(ncol(values))],
            xlab = "Date", ylab = "Adjusted price (native index units)", main = "Aligned asset prices")
    legend("topleft", legend = colnames(values), col = c("#1B4965", "#CA6702", "#4F772D", "#6D597A")[seq_len(ncol(values))],
           lty = 1, bty = "n")
    grid(col = "#E5E7EB")
  })
}

#' Plot PIT dependence
#' @param u Observed PIT matrix.
#' @param simulated Simulated copula values.
#' @param path Output path.
#' @export
plot_copula_diagnostic <- function(u, simulated, path) {
  write_png(path, {
    par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
    plot(u[, 1], u[, 2], pch = 16, cex = 0.55, col = grDevices::adjustcolor("#1B4965", 0.45),
         xlab = "Asset 1 PIT", ylab = "Asset 2 PIT", main = "Empirical PIT dependence")
    grid(col = "#E5E7EB")
    plot(simulated[, 1], simulated[, 2], pch = 16, cex = 0.55, col = grDevices::adjustcolor("#CA6702", 0.45),
         xlab = "Asset 1 simulated U", ylab = "Asset 2 simulated U", main = "Selected copula simulation")
    grid(col = "#E5E7EB")
  })
}

#' Plot rolling VaR exceedances
#' @param forecasts Rolling forecast table.
#' @param path Output path.
#' @param confidence Confidence level.
#' @param model Optional model; defaults to first.
#' @export
plot_var_exceedances <- function(forecasts, path, confidence = 0.99, model = NULL) {
  x <- forecasts[forecasts$confidence == confidence & forecasts$status == "ok", , drop = FALSE]
  if (is.null(model)) model <- unique(x$model)[[1]]
  x <- x[x$model == model, , drop = FALSE]
  if (!nrow(x)) stop("No successful forecasts to plot.", call. = FALSE)
  write_png(path, {
    ylim <- range(c(x$realised_return, -x$var), finite = TRUE)
    plot(as.Date(x$forecast_date), x$realised_return, type = "h", col = "#64748B", lwd = 1,
         xlab = "Forecast date", ylab = "Return", ylim = ylim,
         main = sprintf("Rolling %.0f%% VaR exceedances - %s", 100 * confidence, model))
    lines(as.Date(x$forecast_date), -x$var, col = "#B91C1C", lwd = 2)
    points(as.Date(x$forecast_date)[x$exceedance], x$realised_return[x$exceedance], pch = 19, col = "#DC2626")
    legend("bottomleft", c("Realised return", "VaR threshold", "Exception"),
           col = c("#64748B", "#B91C1C", "#DC2626"), lty = c(1, 1, NA), pch = c(NA, NA, 19), bty = "n")
    grid(col = "#E5E7EB")
  })
}

#' Plot portfolio simulation and risk thresholds
#' @param simulation Simulation result.
#' @param path Output path.
#' @export
plot_simulation_risk <- function(simulation, path) {
  write_png(path, {
    x <- simulation$portfolio_simple_returns
    hist(x, breaks = "FD", col = "#D9EAF1", border = "white", probability = TRUE,
         xlab = "Portfolio simple return", main = "Copula-based portfolio return simulation")
    for (i in seq_len(nrow(simulation$risk))) abline(v = -simulation$risk$var[i],
      col = c("#CA6702", "#B91C1C")[min(i, 2)], lwd = 2, lty = i)
    legend("topleft", sprintf("%.0f%% VaR", 100 * simulation$risk$confidence),
           col = c("#CA6702", "#B91C1C")[seq_len(min(2, nrow(simulation$risk)))],
           lwd = 2, lty = seq_len(nrow(simulation$risk)), bty = "n")
    grid(col = "#E5E7EB")
  })
}

#' Plot model comparison quantile loss
#' @param comparison Model comparison table.
#' @param path Output path.
#' @export
plot_model_comparison <- function(comparison, path) {
  write_png(path, {
    x <- comparison[is.finite(comparison$quantile_loss), ]
    labels <- paste(gsub("_", " ", x$model), sprintf("%.0f%%", 100 * x$confidence), sep = "\n")
    par(mar = c(10, 9, 4, 2) + 0.1, mgp = c(5, 1, 0))
    barplot(x$quantile_loss, names.arg = labels, las = 2, col = "#1B4965",
      ylab = "Mean quantile loss", main = "Out-of-sample model comparison",
      cex.names = 0.72, cex.axis = 0.9)
    grid(nx = NA, ny = NULL, col = "#E5E7EB")
  })
}

#' Plot Monte Carlo convergence
#' @param summary Convergence summary table.
#' @param path Output path.
#' @export
plot_monte_carlo_convergence <- function(summary, path) {
  write_png(path, {
    confidence_levels <- sort(unique(summary$confidence))
    colours <- c("#1B4965", "#B91C1C", "#4F772D", "#6D597A")
    ylim <- range(c(summary$mean_var - summary$sd_var, summary$mean_var + summary$sd_var), finite = TRUE)
    plot(NA, xlim = range(summary$n_simulations), ylim = ylim, log = "x",
         xlab = "Simulation count (log scale)", ylab = "VaR estimate",
         main = "Monte Carlo VaR convergence across repeated seeds")
    for (i in seq_along(confidence_levels)) {
      x <- summary[summary$confidence == confidence_levels[i], ]
      lines(x$n_simulations, x$mean_var, type = "b", pch = 19, lwd = 2, col = colours[i])
      segments(x$n_simulations, x$mean_var - x$sd_var, x$n_simulations,
               x$mean_var + x$sd_var, col = colours[i])
    }
    legend("topright", sprintf("%.0f%% VaR", 100 * confidence_levels),
           col = colours[seq_along(confidence_levels)], lty = 1, pch = 19, bty = "n")
    grid(col = "#E5E7EB")
  })
}
