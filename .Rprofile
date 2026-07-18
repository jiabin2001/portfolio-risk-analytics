local({
  project <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  Sys.setenv(
    RENV_PATHS_ROOT = file.path(project, "renv", "root"),
    RENV_PATHS_LIBRARY_ROOT = file.path(project, "renv", "library"),
    RENV_PATHS_CACHE = file.path(project, "renv", "cache"),
    RENV_PATHS_SANDBOX = file.path(project, "renv", "sandbox"),
    RENV_PATHS_CELLAR = file.path(project, "renv", "cellar"),
    RENV_CONFIG_CACHE_ENABLED = "FALSE"
  )
  activate <- file.path("renv", "activate.R")
  if (file.exists(activate)) source(activate)
})
