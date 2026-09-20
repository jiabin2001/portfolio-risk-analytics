#' Paired circular block bootstrap for a loss difference
#'
#' Blocks retain short-range serial dependence. The percentile interval is marginal;
#' the two-sided p-value uses bootstrap differences centred at the observed mean.
#' Inference assumes a sufficiently stable loss-difference process and does not
#' account for selecting models or hyperparameters using the evaluation sample.
#' @param loss_difference Model score minus reference score, in time order.
#' @param bootstrap_replications Number of resamples, at least 99.
#' @param block_length Block length, default ceiling(n^(1/3)).
#' @param confidence_interval Coverage of the marginal bootstrap interval.
#' @param seed Reproducible random seed; caller RNG state is preserved.
#' @return Named inference result. At least 20 observations and two blocks are required.
#' @export
paired_block_bootstrap <- function(loss_difference, bootstrap_replications = 999L,
                                   block_length = NULL, confidence_interval = 0.95, seed = 20260718L) {
  difference <- as.numeric(loss_difference)
  if (any(!is.finite(difference))) stop("Loss differences must be finite and aligned.", call. = FALSE)
  assert_scalar(bootstrap_replications, "bootstrap_replications", "numeric")
  if (bootstrap_replications < 99L || bootstrap_replications != floor(bootstrap_replications)) {
    stop("bootstrap_replications must be an integer of at least 99.", call. = FALSE)
  }
  validate_confidence(confidence_interval)
  assert_scalar(seed, "seed", "numeric")
  if (seed < 0 || seed > .Machine$integer.max || seed != floor(seed)) {
    stop("seed must be a nonnegative R integer.", call. = FALSE)
  }
  n <- length(difference)
  if (is.null(block_length)) block_length <- max(1L, ceiling(n^(1 / 3)))
  assert_scalar(block_length, "block_length", "numeric")
  if (block_length < 1L || block_length != floor(block_length)) {
    stop("block_length must be a positive integer.", call. = FALSE)
  }
  result <- list(observations = n, mean_loss_difference = score_sample_mean(difference),
                 ci_lower = NA_real_, ci_upper = NA_real_, p_value_bootstrap = NA_real_,
                 block_length = as.integer(block_length), bootstrap_replications = as.integer(bootstrap_replications),
                 confidence_interval = confidence_interval, seed = seed, status = "insufficient_observations")
  if (n < max(20L, 2L * block_length)) return(result)
  bootstrap_means <- with_preserved_seed(seed, replicate(bootstrap_replications, {
    starts <- sample.int(n, ceiling(n / block_length), replace = TRUE)
    indices <- unlist(lapply(starts, function(start) {
      ((start - 1L + seq_len(block_length) - 1L) %% n) + 1L
    }), use.names = FALSE)[seq_len(n)]
    mean(difference[indices])
  }))
  tail <- (1 - confidence_interval) / 2
  interval <- stats::quantile(bootstrap_means, c(tail, 1 - tail), names = FALSE)
  result$ci_lower <- interval[[1]]
  result$ci_upper <- interval[[2]]
  result$p_value_bootstrap <- (1 + sum(abs(bootstrap_means - mean(difference)) >= abs(mean(difference)))) /
    (bootstrap_replications + 1)
  result$status <- "ok"
  result
}

#' Compare predictive loss against a reference model
#'
#' Each confidence level and scoring rule uses the intersection of valid forecast
#' dates across all supplied models. Negative differences favour the model being
#' compared. Coverage and missing forecasts are reported separately by
#' [compare_forecast_models()]. If the common dates have internal gaps in the
#' supplied forecast calendar, point differences are retained but bootstrap
#' inference is unavailable: missing dates are never treated as adjacent periods.
#' Holm-adjusted p-values cover all comparisons in this call; intervals remain
#' marginal and are not simultaneous confidence intervals.
#' @param forecasts Common-schema forecast table, including forecast_date.
#' @param reference_model Benchmark model present at each confidence level.
#' @param bootstrap_replications Number of paired block resamples.
#' @param block_length Block length, or NULL for ceiling(n^(1/3)).
#' @param confidence_interval Coverage of marginal bootstrap intervals.
#' @param seed Base bootstrap seed.
#' @return List with machine-readable summary and per-date detail tables and metadata.
#' @export
compare_predictive_ability <- function(forecasts, reference_model = "historical",
                                      bootstrap_replications = 999L, block_length = NULL,
                                      confidence_interval = 0.95, seed = 20260718L) {
  forecasts <- prepare_forecast_scores(forecasts)
  assert_scalar(reference_model, "reference_model", "character")
  assert_scalar(seed, "seed", "numeric")
  if (seed < 0 || seed > .Machine$integer.max || seed != floor(seed)) {
    stop("seed must be a nonnegative R integer.", call. = FALSE)
  }
  # Validate bootstrap settings even when the table contains no model pair.
  invisible(paired_block_bootstrap(numeric(), bootstrap_replications, block_length, confidence_interval, seed))
  summary_rows <- detail_rows <- list()
  pair_id <- 0L
  for (confidence in sort(unique(forecasts$confidence))) {
    group <- forecasts[forecasts$confidence == confidence, , drop = FALSE]
    if (!reference_model %in% group$model) {
      stop(sprintf("Reference model '%s' is absent at confidence %s.", reference_model, confidence), call. = FALSE)
    }
    calendar <- sort(unique(group$forecast_date))
    for (metric in c("quantile_loss", "joint_var_es")) {
      column <- if (metric == "quantile_loss") ".quantile_loss" else ".joint_score"
      dates <- common_score_dates(group, column)
      positions <- match(dates, as.character(calendar))
      reference <- group[group$model == reference_model, , drop = FALSE]
      reference_scores <- reference[[column]][match(dates, as.character(reference$forecast_date))]
      for (model in sort(setdiff(unique(group$model), reference_model))) {
        pair_id <- pair_id + 1L
        current <- group[group$model == model, , drop = FALSE]
        model_scores <- current[[column]][match(dates, as.character(current$forecast_date))]
        differences <- model_scores - reference_scores
        local_seed <- (as.double(seed) + pair_id - 1) %% .Machine$integer.max
        has_gaps <- length(positions) > 1L && any(diff(positions) != 1L)
        inference <- paired_block_bootstrap(
          if (has_gaps) numeric() else differences, bootstrap_replications, block_length,
          confidence_interval, local_seed
        )
        inference$observations <- length(differences)
        inference$mean_loss_difference <- score_sample_mean(differences)
        if (has_gaps) {
          inference$status <- "noncontiguous_common_sample"
          inference$block_length <- as.integer(block_length %||% max(1L, ceiling(length(differences)^(1 / 3))))
        }
        interpretation <- if (inference$status != "ok") "inference_unavailable" else if (inference$ci_upper < 0) {
          "lower_loss_marginal_interval"
        } else if (inference$ci_lower > 0) "higher_loss_marginal_interval" else "inconclusive"
        summary_rows[[length(summary_rows) + 1L]] <- cbind(
          data.frame(model = model, reference_model = reference_model, confidence = confidence,
                     score_type = metric, forecast_dates = length(calendar),
                     common_sample_fraction = length(dates) / length(calendar), stringsAsFactors = FALSE),
          as.data.frame(inference, stringsAsFactors = FALSE),
          data.frame(interpretation = interpretation, stringsAsFactors = FALSE)
        )
        if (length(dates)) detail_rows[[length(detail_rows) + 1L]] <- data.frame(
          model = model, reference_model = reference_model, confidence = confidence,
          score_type = metric, forecast_date = as.Date(dates), calendar_index = positions,
          model_score = model_scores, reference_score = reference_scores,
          loss_difference = differences, stringsAsFactors = FALSE
        )
      }
    }
  }
  summary <- if (length(summary_rows)) do.call(rbind, summary_rows) else data.frame(
    model = character(), reference_model = character(), confidence = numeric(), score_type = character(),
    forecast_dates = integer(), common_sample_fraction = numeric(), observations = integer(),
    mean_loss_difference = numeric(), ci_lower = numeric(), ci_upper = numeric(), p_value_bootstrap = numeric(),
    block_length = integer(), bootstrap_replications = integer(), confidence_interval = numeric(),
    seed = numeric(), status = character(), interpretation = character()
  )
  summary$p_value_holm <- stats::p.adjust(summary$p_value_bootstrap, method = "holm")
  detail <- if (length(detail_rows)) do.call(rbind, detail_rows) else data.frame(
    model = character(), reference_model = character(), confidence = numeric(), score_type = character(),
    forecast_date = as.Date(character()), calendar_index = integer(), model_score = numeric(),
    reference_score = numeric(), loss_difference = numeric()
  )
  row.names(summary) <- row.names(detail) <- NULL
  list(summary = summary, detail = detail, metadata = list(
    method = "paired_circular_block_bootstrap", difference = "model_minus_reference",
    interval = "marginal_percentile", multiple_testing = "Holm across this call",
    sample = "intersection across models within confidence and scoring rule", seed = seed
  ))
}
