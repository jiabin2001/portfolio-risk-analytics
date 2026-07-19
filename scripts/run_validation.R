script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_arg)) dirname(normalizePath(sub("^--file=", "", script_arg[[1]]))) else "scripts"
source(file.path(script_dir, "bootstrap.R"))
setwd(.PRA_PROJECT_ROOT)

started <- Sys.time()
result <- run_analysis("config/validation.yml", run_copula_rolling = FALSE)
cat(sprintf("Validation run completed: %s\n", result$run_id))
cat(sprintf("Elapsed seconds: %.3f\n", as.numeric(difftime(Sys.time(), started, units = "secs"))))
print(result$simulation$risk)
print(result$comparison)
