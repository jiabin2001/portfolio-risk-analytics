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
    colours <- grDevices::hcl.colors(ncol(values), "Dark 3")
    matplot(aligned_prices$date, values, type = "l", lty = 1, lwd = 1.5,
            col = colours,
            xlab = "Date", ylab = "Adjusted price (native index units)", main = "Aligned asset prices")
    legend("topleft", legend = colnames(values), col = colours, lty = 1, bty = "n")
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
  if (!nrow(x)) return(write_png(path, {
    plot.new(); text(0.5, 0.5, "No successful forecasts at this confidence level")
  }))
  if (is.null(model)) model <- unique(x$model)[[1]]
  x <- x[x$model == model, , drop = FALSE]
  if (!nrow(x)) stop("No successful forecasts to plot.", call. = FALSE)
  display_model <- gsub("_", "-", model, fixed = TRUE)
  threshold <- -(x$loss_var %||% x$var)
  write_png(path, {
    ylim <- range(c(x$realised_return, threshold), finite = TRUE)
    plot(as.Date(x$forecast_date), x$realised_return, type = "h", col = "#64748B", lwd = 1,
         xlab = "Forecast date", ylab = "Return", ylim = ylim,
         main = sprintf("Rolling %.0f%% VaR exceedances - %s", 100 * confidence, display_model))
    lines(as.Date(x$forecast_date), threshold, col = "#B91C1C", lwd = 2)
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
    x <- simulation$portfolio_model_returns %||% simulation$portfolio_simple_returns
    return_type <- simulation$metadata$return_type %||% "simple"
    hist(x, breaks = "FD", col = "#D9EAF1", border = "white", probability = TRUE,
         xlab = sprintf("Portfolio %s return", return_type), main = "Copula-based portfolio return simulation")
    for (i in seq_len(nrow(simulation$risk))) abline(v = -(simulation$risk$loss_var %||% simulation$risk$var)[i],
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
    if (!nrow(x)) {
      plot.new(); text(0.5, 0.5, "No common valid forecasts available")
    } else {
      levels <- sort(unique(x$confidence))
      par(mfrow = c(1, length(levels)), mar = c(5, 10, 4, 2))
      for (level in levels) {
        panel <- x[x$confidence == level, ]
        panel <- panel[order(panel$quantile_loss, decreasing = TRUE), ]
        colours <- ifelse(panel$model == "copula_garch", "#CA6702", "#1B4965")
        barplot(panel$quantile_loss, names.arg = gsub("_", " ", panel$model),
          horiz = TRUE, las = 1, col = colours, border = NA,
          xlab = "Mean quantile loss (common dates)", main = sprintf("%.0f%% VaR", 100 * level),
          cex.names = 0.85)
        grid(nx = NULL, ny = NA, col = "#E5E7EB")
      }
    }
  }, width = 1600, height = 900)
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
