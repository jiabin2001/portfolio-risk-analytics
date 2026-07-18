script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_arg)) dirname(normalizePath(sub("^--file=", "", script_arg[[1]]))) else "scripts"
source(file.path(script_dir, "bootstrap.R"))
setwd(.PRA_PROJECT_ROOT)

if (!requireNamespace("testthat", quietly = TRUE)) stop("Install project dependencies before running tests.", call. = FALSE)
result <- testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE, stop_on_warning = FALSE)
invisible(result)
