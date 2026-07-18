#' Append a structured project log event
#' @param level Log level.
#' @param stage Pipeline stage.
#' @param message Human-readable message.
#' @param run_id Run identifier.
#' @param root Project root.
#' @return Log path.
#' @export
log_event <- function(level, stage, message, run_id = "latest", root = pra_project_root()) {
  dirs <- ensure_project_dirs(root)
  path <- file.path(dirs[["logs"]], paste0(run_id, ".tsv"))
  row <- data.frame(
    timestamp_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    level = as.character(level), stage = as.character(stage),
    message = gsub("[\r\n\t]+", " ", as.character(message)),
    stringsAsFactors = FALSE
  )
  write.table(row, path, append = file.exists(path), sep = "\t", row.names = FALSE,
              col.names = !file.exists(path), quote = TRUE)
  invisible(path)
}

#' Build a deterministic run identifier
#' @param profile Profile name.
#' @param when Timestamp.
#' @export
make_run_id <- function(profile, when = Sys.time()) {
  paste0(gsub("[^A-Za-z0-9_-]", "_", profile), "_", format(when, "%Y%m%dT%H%M%S"))
}
