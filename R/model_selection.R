#' Select a marginal model by validity constraints then information criterion
#' @param fits Structured fit results.
#' @param diagnostic_alpha Minimum diagnostic p-value.
#' @param criterion `AIC` or `BIC`.
#' @return Selection object with complete comparison table.
#' @export
select_marginal_model <- function(fits, diagnostic_alpha = 0.01, criterion = c("BIC", "AIC")) {
  criterion <- match.arg(toupper(criterion), c("BIC", "AIC"))
  diagnostics <- lapply(fits, function(fit) {
    if (!identical(fit$status, "ok")) return(NULL)
    tryCatch(diagnose_marginal(fit), error = function(e) structure(list(error = conditionMessage(e)), class = "diagnostic_error"))
  })
  rows <- lapply(seq_along(fits), function(i) {
    fit <- fits[[i]]; diag <- diagnostics[[i]]
    diag_ok <- !is.null(diag) && !inherits(diag, "diagnostic_error")
    residual_p <- if (diag_ok) diag$residual_ljung_box_p_value else NA_real_
    squared_p <- if (diag_ok) diag$squared_ljung_box_p_value else NA_real_
    pit_p <- if (diag_ok) diag$pit_ks_p_value else NA_real_
    finite_diagnostics <- all(is.finite(c(residual_p, squared_p, pit_p)))
    passed <- identical(fit$status, "ok") && diag_ok && finite_diagnostics &&
      residual_p >= diagnostic_alpha && squared_p >= diagnostic_alpha && pit_p >= diagnostic_alpha
    reason <- if (!identical(fit$status, "ok")) fit$error %||% "fit_failed" else if (!diag_ok) {
      if (inherits(diag, "diagnostic_error")) diag$error else "diagnostics_unavailable"
    } else if (!finite_diagnostics) {
      "non_finite_diagnostics"
    } else paste(c(if (residual_p < diagnostic_alpha) "residual_autocorrelation",
                   if (squared_p < diagnostic_alpha) "remaining_arch",
                   if (pit_p < diagnostic_alpha) "pit_nonuniform"), collapse = ";")
    data.frame(
      model_id = fit$model_id, engine = fit$engine %||% NA_character_,
      arma_p = fit$spec$arma_p, arma_q = fit$spec$arma_q,
      garch_p = fit$spec$garch_p, garch_q = fit$spec$garch_q,
      volatility_family = fit$spec$volatility_family,
      distribution = fit$spec$distribution,
      convergence = isTRUE(fit$converged), parameter_valid = isTRUE(fit$parameter_valid),
      aic = fit$aic %||% NA_real_, bic = fit$bic %||% NA_real_,
      residual_ljung_box_p = residual_p, squared_residual_ljung_box_p = squared_p,
      pit_ks_p = pit_p, diagnostics_passed = passed,
      rejection_reason = if (passed) "" else reason,
      runtime_seconds = fit$runtime_seconds %||% NA_real_, stringsAsFactors = FALSE
    )
  })
  table <- do.call(rbind, rows)
  criterion_col <- tolower(criterion)
  eligible <- which(table$diagnostics_passed & is.finite(table[[criterion_col]]))
  fallback <- FALSE
  warning_message <- NA_character_
  if (!length(eligible)) {
    eligible <- which(table$convergence & table$parameter_valid & is.finite(table[[criterion_col]]))
    fallback <- TRUE
    warning_message <- "No candidate passed all diagnostics; selected the best converged IC-ranked fallback."
  }
  if (length(eligible) > 1L && all(table$engine[eligible] == "native_two_step")) {
    orders <- unique(table[eligible, c("arma_p", "arma_q"), drop = FALSE])
    orders <- orders[order(orders$arma_p + orders$arma_q, orders$arma_p, orders$arma_q), , drop = FALSE]
    selected_order <- orders[1L, ]
    eligible <- eligible[
      table$arma_p[eligible] == selected_order$arma_p & table$arma_q[eligible] == selected_order$arma_q
    ]
    native_warning <- sprintf(
      "Native two-step IC comparison was restricted to ARMA(%d,%d); IC values across ARMA orders are approximate.",
      selected_order$arma_p, selected_order$arma_q
    )
    warning_message <- paste(na.omit(c(warning_message, native_warning)), collapse = " ")
  }
  if (!length(eligible)) {
    table$rank <- NA_integer_; table$selected <- FALSE
    return(list(selected = NULL, table = table, fallback = TRUE,
                warning = "No valid marginal model was available.", diagnostics = diagnostics))
  }
  order_idx <- eligible[order(table[[criterion_col]][eligible],
                              table$aic[eligible], table$model_id[eligible])]
  table$rank <- NA_integer_
  table$rank[order_idx] <- seq_along(order_idx)
  table$selected <- FALSE
  table$selected[order_idx[[1]]] <- TRUE
  list(selected = fits[[order_idx[[1]]]], table = table, fallback = fallback,
       warning = warning_message, diagnostics = diagnostics)
}
