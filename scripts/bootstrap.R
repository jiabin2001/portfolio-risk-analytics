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

bootstrap_source <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
bootstrap_start <- if (!is.null(bootstrap_source) && nzchar(bootstrap_source)) {
  dirname(dirname(normalizePath(bootstrap_source, winslash = "/", mustWork = TRUE)))
} else {
  getwd()
}
.PRA_PROJECT_ROOT <- bootstrap_project(bootstrap_start)
active_project <- Sys.getenv("RENV_PROJECT", unset = "")
active_project <- if (nzchar(active_project)) normalizePath(active_project, winslash = "/", mustWork = FALSE) else ""
if (!identical(active_project, .PRA_PROJECT_ROOT)) {
  project_profile <- file.path(.PRA_PROJECT_ROOT, ".Rprofile")
  if (file.exists(project_profile)) sys.source(project_profile, envir = .GlobalEnv, chdir = TRUE)
}
invisible(.PRA_PROJECT_ROOT)
