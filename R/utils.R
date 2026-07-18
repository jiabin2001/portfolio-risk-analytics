#' Null-coalescing helper
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Assert a scalar value
#' @keywords internal
assert_scalar <- function(x, name, type = c("numeric", "character", "logical")) {
  type <- match.arg(type)
  ok <- length(x) == 1L && !is.na(x) && switch(
    type,
    numeric = is.numeric(x) && is.finite(x),
    character = is.character(x) && nzchar(x),
    logical = is.logical(x)
  )
  if (!ok) stop(sprintf("`%s` must be one finite %s value.", name, type), call. = FALSE)
  invisible(x)
}

#' Locate the project root
#'
#' Walks upward until `DESCRIPTION` is found. No absolute path is stored.
#' @param start Directory from which to search.
#' @return Normalised project-root path.
#' @export
pra_project_root <- function(start = getwd()) {
  path <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(path, "DESCRIPTION"))) return(path)
    parent <- dirname(path)
    if (identical(parent, path)) stop("Could not locate project root.", call. = FALSE)
    path <- parent
  }
}

#' Construct a project-relative path
#' @param ... Path components below the project root.
#' @param root Project root.
#' @export
project_path <- function(..., root = pra_project_root()) file.path(root, ...)

#' Create all project runtime directories
#' @param root Project root.
#' @return Named character vector of directories.
#' @export
ensure_project_dirs <- function(root = pra_project_root()) {
  rel <- c(
    raw = "data/raw", processed = "data/processed", cache = "data/cache",
    fixtures = "data/fixtures", figures = "outputs/figures",
    tables = "outputs/tables", models = "outputs/models",
    diagnostics = "outputs/diagnostics", backtests = "outputs/backtests",
    logs = "outputs/logs", checkpoints = "outputs/checkpoints"
  )
  paths <- file.path(root, rel)
  names(paths) <- names(rel)
  invisible(vapply(paths, dir.create, logical(1), recursive = TRUE, showWarnings = FALSE))
  paths
}

#' Return a safe worker count
#' @param requested Optional configured count.
#' @param detected Detected logical cores (injectable for tests).
#' @export
safe_worker_count <- function(requested = NULL, detected = parallel::detectCores(logical = TRUE)) {
  if (!is.finite(detected) || detected < 1) detected <- 1L
  cap <- max(1L, min(floor(detected / 2), detected - 2L))
  if (is.null(requested)) return(as.integer(cap))
  if (length(requested) != 1L || !is.numeric(requested) || !is.finite(requested) || requested < 1) {
    stop("`requested` workers must be a positive integer or NULL.", call. = FALSE)
  }
  as.integer(min(requested, cap))
}

#' Time an expression
#' @param expr Expression to evaluate.
#' @return List with value and elapsed seconds.
#' @export
time_stage <- function(expr) {
  started <- proc.time()[["elapsed"]]
  value <- force(expr)
  list(value = value, elapsed_seconds = unname(proc.time()[["elapsed"]] - started))
}

#' Stable log helper
#' @keywords internal
xlogy <- function(x, y) {
  size <- max(length(x), length(y))
  x <- rep_len(x, size); y <- rep_len(y, size)
  out <- numeric(size)
  use <- x != 0
  out[use] <- x[use] * log(y[use])
  out
}

#' Atomic RDS writer
#' @param object Object to save.
#' @param path Destination.
#' @export
atomic_save_rds <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  saveRDS(object, tmp)
  if (file.exists(path)) unlink(path)
  if (!file.rename(tmp, path)) stop("Could not atomically move checkpoint into place.", call. = FALSE)
  invisible(path)
}

#' Model-stage dependency fingerprint
#' @noRd
analysis_fingerprint <- function(config_path, model_returns) {
  x <- as.matrix(model_returns)
  paste(
    unname(tools::md5sum(config_path)), nrow(x), ncol(x),
    format(sum(x), digits = 17), format(sum(x^2), digits = 17),
    format(sum(abs(x)), digits = 17), sep = "|"
  )
}
