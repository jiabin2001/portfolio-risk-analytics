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
#' @return Manifest path.
#' @export
write_output_manifest <- function(files, run_id, root = pra_project_root()) {
  absolute_paths <- normalizePath(unname(files), winslash = "/", mustWork = FALSE)
  root_path <- normalizePath(root, winslash = "/", mustWork = TRUE)
  prefix <- paste0(root_path, "/")
  relative_paths <- vapply(absolute_paths, function(path) {
    if (startsWith(path, prefix)) substring(path, nchar(prefix) + 1L) else path
  }, character(1))
  manifest <- data.frame(
    artifact = names(files), path = relative_paths, exists = file.exists(absolute_paths),
    generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE), run_id = run_id,
    stringsAsFactors = FALSE
  )
  write_output_table(manifest, "output_manifest", root)
}
