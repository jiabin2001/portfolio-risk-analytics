source("scripts/bootstrap.R")

if (!requireNamespace("testthat", quietly = TRUE)) stop("Install project dependencies before running tests.", call. = FALSE)
result <- testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE, stop_on_warning = FALSE)
invisible(result)
