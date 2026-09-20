# Verify published artifacts without loading the package, restoring dependencies,
# fetching data, or fitting models. Run with Rscript --vanilla scripts/audit_outputs.R.
audit_output_artifacts <- function(root) {
  fail <- function(message) stop(paste0("Artifact audit failed: ", message), call. = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  outputs <- file.path(root, "outputs")
  manifest_path <- file.path(outputs, "tables", "output_manifest.csv")
  if (!file.exists(manifest_path)) fail("outputs/tables/output_manifest.csv is missing.")
  read_table <- function(path) {
    tryCatch(utils::read.csv(path, colClasses = "character", check.names = FALSE,
                            stringsAsFactors = FALSE, na.strings = character()),
             error = function(e) fail(sprintf("Cannot read %s: %s", basename(path), conditionMessage(e))))
  }
  manifest <- read_table(manifest_path)
  required <- c("artifact", "path", "exists", "run_id", "md5", "bytes")
  if (!nrow(manifest) || !all(required %in% names(manifest)) || anyDuplicated(names(manifest))) {
    fail("Manifest is empty or does not have the required unique columns.")
  }
  for (column in required) {
    if (anyNA(manifest[[column]]) || any(!nzchar(manifest[[column]]))) {
      fail(sprintf("Manifest column '%s' contains missing values.", column))
    }
  }
  # A table and a figure may share a semantic label; resolved paths identify files.
  if (length(unique(manifest$run_id)) != 1L) fail("Manifest mixes multiple run IDs.")
  if (any(manifest$exists != "TRUE")) fail("Manifest contains an artifact not recorded as existing.")
  if (any(!grepl("^[[:xdigit:]]{32}$", manifest$md5))) fail("Manifest contains invalid MD5 values.")
  expected_bytes <- suppressWarnings(as.numeric(manifest$bytes))
  if (any(!is.finite(expected_bytes)) || any(expected_bytes < 0 | expected_bytes != floor(expected_bytes))) {
    fail("Manifest byte counts must be finite nonnegative integers.")
  }

  # Enforce portable repository-relative paths before resolving real paths, then
  # check containment again to reject symlinks/junctions outside outputs/.
  relative <- gsub("\\", "/", manifest$path, fixed = TRUE)
  invalid <- grepl("^/|^[A-Za-z]:", relative) | !startsWith(relative, "outputs/")
  invalid <- invalid | vapply(strsplit(relative, "/", fixed = TRUE), function(parts) {
    any(parts %in% c("", ".", ".."))
  }, logical(1))
  if (any(invalid)) fail(sprintf("Artifact path must stay within repository outputs/: %s", manifest$path[which(invalid)[1L]]))
  paths <- file.path(root, relative)
  missing <- !file.exists(paths) | dir.exists(paths)
  if (any(missing)) fail(sprintf("Artifact file is missing: %s", relative[which(missing)[1L]]))
  paths <- normalizePath(paths, winslash = "/", mustWork = TRUE)
  outputs <- normalizePath(outputs, winslash = "/", mustWork = TRUE)
  path_keys <- if (.Platform$OS.type == "windows") tolower(paths) else paths
  output_key <- if (.Platform$OS.type == "windows") tolower(outputs) else outputs
  root_key <- if (.Platform$OS.type == "windows") tolower(root) else root
  if (!startsWith(output_key, paste0(root_key, "/"))) fail("The outputs directory resolves outside the repository.")
  if (any(!startsWith(path_keys, paste0(output_key, "/")))) fail("An artifact resolves outside repository outputs/.")
  if (anyDuplicated(path_keys)) fail("Manifest contains duplicated artifact paths.")
  observed_bytes <- file.info(paths)$size
  observed_md5 <- unname(tools::md5sum(paths))
  bad <- is.na(observed_md5) | tolower(observed_md5) != tolower(manifest$md5) |
    is.na(observed_bytes) | observed_bytes != expected_bytes
  if (any(bad)) fail(sprintf("Checksum or byte count mismatch: %s", paste(relative[bad], collapse = ", ")))

  provenance_index <- which(manifest$artifact == "run_provenance")
  if (length(provenance_index) != 1L) fail("Manifest must contain exactly one run_provenance artifact.")
  provenance <- read_table(paths[provenance_index])
  if (!all(c("key", "value") %in% names(provenance)) || anyDuplicated(names(provenance)) ||
      anyNA(provenance$key) || any(!nzchar(provenance$key)) || anyDuplicated(provenance$key)) {
    fail("Run provenance must have unique nonempty keys and a value column.")
  }
  values <- stats::setNames(provenance$value, provenance$key)
  if (!"run_id" %in% names(values) || is.na(values[["run_id"]]) ||
      !identical(unname(values[["run_id"]]), manifest$run_id[[1L]])) {
    fail("Manifest run_id does not match run provenance.")
  }
  for (field in intersect(c("git_commit", "source_dirty", "config_hash", "input_hash", "implementation_hash"), names(manifest))) {
    if (!field %in% names(values) || is.na(values[[field]]) || anyNA(manifest[[field]]) ||
        any(manifest[[field]] != values[[field]])) {
      fail(sprintf("Manifest %s does not match run provenance.", field))
    }
  }
  cat(sprintf("Artifact audit passed: %d files, %s bytes, run %s.\n",
              nrow(manifest), format(sum(observed_bytes), scientific = FALSE, trim = TRUE), manifest$run_id[[1L]]))
  invisible(list(run_id = manifest$run_id[[1L]], artifacts = nrow(manifest), bytes = sum(observed_bytes)))
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 1L) stop("Usage: Rscript --vanilla scripts/audit_outputs.R [repository-root]", call. = FALSE)
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]), winslash = "/", mustWork = TRUE)
  root <- if (length(args)) args[[1L]] else dirname(dirname(script_path))
  audit_output_artifacts(root)
}
