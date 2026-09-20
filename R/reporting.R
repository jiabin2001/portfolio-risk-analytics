#' Persist a data frame as a report table
#' @param x Data frame.
#' @param name Stable table name without extension.
#' @param root Project root.
#' @return Output path.
#' @export
write_output_table <- function(x, name, root = pra_project_root()) {
  paths <- ensure_project_dirs(root)
  path <- file.path(paths[["tables"]], paste0(name, ".csv"))
  write.csv(x, path, row.names = FALSE, na = "")
  path
}

#' Build an output manifest for the report
#' @param files Named output paths.
#' @param run_id Run identifier.
#' @param root Project root.
#' @param provenance Optional named source and configuration identifiers.
#' @return Manifest path.
#' @export
write_output_manifest <- function(files, run_id, root = pra_project_root(), provenance = NULL) {
  absolute_paths <- normalizePath(unname(files), winslash = "/", mustWork = FALSE)
  root_path <- normalizePath(root, winslash = "/", mustWork = TRUE)
  prefix <- paste0(root_path, "/")
  relative_paths <- vapply(absolute_paths, function(path) {
    if (startsWith(path, prefix)) substring(path, nchar(prefix) + 1L) else path
  }, character(1))
  manifest <- data.frame(
    artifact = names(files), path = relative_paths, exists = file.exists(absolute_paths),
    generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE), run_id = run_id,
    md5 = unname(tools::md5sum(absolute_paths)), bytes = file.info(absolute_paths)$size,
    stringsAsFactors = FALSE
  )
  if (!is.null(provenance)) for (name in names(provenance)) manifest[[name]] <- as.character(provenance[[name]])
  write_output_table(manifest, "output_manifest", root)
}

#' Capture the source revision without depending on Git being installed
#' @keywords internal
source_revision <- function(root) {
  git <- Sys.which("git")
  if (!nzchar(git)) return(list(git_commit = "unavailable", source_dirty = "unknown"))
  run <- function(args) tryCatch(suppressWarnings(system2(git,
    c("-C", shQuote(normalizePath(root, winslash = "/")), args), stdout = TRUE, stderr = FALSE)),
    error = function(e) character())
  revision <- run(c("rev-parse", "HEAD"))
  dirty <- run(c("status", "--porcelain", "--", "R", "config", "scripts", "DESCRIPTION", "renv.lock", "_targets.R"))
  list(git_commit = if (length(revision) && is.null(attr(revision, "status"))) revision[[1]] else "unavailable",
       source_dirty = if (!is.null(attr(dirty, "status"))) "unknown" else as.character(length(dirty) > 0L))
}

#' Write run provenance and freeze the inputs in a local run archive
#' @keywords internal
write_run_provenance <- function(cfg, acquired, run_id, source_state, root) {
  archive <- file.path(root, "outputs", "runs", run_id)
  if (dir.exists(archive)) stop("Run archive already exists; refusing to overwrite it.", call. = FALSE)
  dir.create(archive, recursive = TRUE)
  file.copy(cfg$config_path, file.path(archive, "config.yml"))
  atomic_save_rds(acquired, file.path(archive, "price_snapshot.rds"))
  versions <- vapply(c("yaml", "rugarch", "VineCopula", "quantmod", "xts", "targets"), function(pkg) {
    if (requireNamespace(pkg, quietly = TRUE)) as.character(utils::packageVersion(pkg)) else "unavailable"
  }, character(1))
  values <- c(list(run_id = run_id, profile = cfg$profile, schema = "3",
    config_hash = unname(tools::md5sum(cfg$config_path)), implementation_hash = implementation_fingerprint(),
    input_hash = acquired$metadata$input_hash %||% stable_object_md5(acquired$prices),
    aligned_hash = acquired$metadata$aligned_hash %||% stable_object_md5(acquired$aligned),
    data_source = acquired$metadata$source, retrieved_at_utc = acquired$metadata$downloaded_at_utc,
    valuation_start = as.character(min(acquired$aligned$date)), valuation_end = as.character(max(acquired$aligned$date)),
    frequency = cfg$data$frequency, observations = nrow(acquired$aligned),
    currency_mode = cfg$portfolio$currency_mode, return_type = cfg$portfolio$return_type,
    r_version = as.character(getRversion()), rng_kind = paste(RNGkind(), collapse = ";"),
    simulation_seed = cfg$simulation$seed, current_draws = cfg$simulation$final_n,
    rolling_draws = cfg$simulation$rolling_n, chunk_size = cfg$simulation$chunk_size),
    source_state, as.list(setNames(versions, paste0("package_", names(versions)))))
  table <- data.frame(key = names(values), value = vapply(values, as.character, character(1)), stringsAsFactors = FALSE)
  path <- write_output_table(table, "run_provenance", root)
  writeLines(utils::capture.output(utils::sessionInfo()), file.path(archive, "session-info.txt"))
  list(path = path, values = values, archive = archive)
}

#' Render the report after verifying its generated inputs
#' @param source Quarto report source path.
#' @param root Project root.
#' @return Rendered HTML path.
#' @export
render_analysis_report <- function(source = "report/portfolio_risk_analytics.qmd", root = pra_project_root()) {
  quarto <- Sys.which("quarto")
  if (!nzchar(quarto)) {
    candidates <- list.files(file.path(root, "tools"), pattern = "quarto[.]exe$", recursive = TRUE, full.names = TRUE)
    if (length(candidates)) quarto <- normalizePath(candidates[[1]], winslash = "/")
  }
  if (!nzchar(quarto)) stop("Install Quarto or place a portable build under tools/ to render the report.", call. = FALSE)
  source <- if (is_absolute_data_path(source)) source else file.path(root, source)
  if (!file.exists(file.path(root, "outputs", "tables", "output_manifest.csv"))) stop("Run the analysis before rendering.", call. = FALSE)
  if (.Platform$OS.type == "windows") {
    # Quarto copies cached CSS into the report. An EFS-encrypted user cache cannot
    # be copied to an unencrypted volume; keep this child-process cache with the project.
    cache <- file.path(root, "tools", "quarto-local-cache")
    dir.create(cache, recursive = TRUE, showWarnings = FALSE)
    previous_appdata <- Sys.getenv("LOCALAPPDATA", unset = NA_character_)
    on.exit({
      if (is.na(previous_appdata)) Sys.unsetenv("LOCALAPPDATA") else
        Sys.setenv(LOCALAPPDATA = previous_appdata)
    }, add = TRUE)
    Sys.setenv(LOCALAPPDATA = normalizePath(cache, winslash = "/", mustWork = TRUE))
  }
  status <- system2(quarto, c("render", shQuote(source), "--to", "html"))
  if (!identical(status, 0L)) stop("Quarto rendering failed.", call. = FALSE)
  normalizePath(sub("[.]qmd$", ".html", source), winslash = "/", mustWork = TRUE)
}
