bootstrap_project <- function(root = getwd()) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  while (!file.exists(file.path(root, "DESCRIPTION"))) {
    parent <- dirname(root)
    if (identical(parent, root)) stop("Could not locate the project root.", call. = FALSE)
    root <- parent
  }
  files <- sort(list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE))
  invisible(lapply(files, sys.source, envir = .GlobalEnv))
  invisible(root)
}

invisible(bootstrap_project())
