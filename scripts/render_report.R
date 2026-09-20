script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_arg)) dirname(normalizePath(sub("^--file=", "", script_arg[[1]]))) else "scripts"
source(file.path(script_dir, "bootstrap.R"))
setwd(.PRA_PROJECT_ROOT)

cat(sprintf("Rendered %s\n", render_analysis_report()))
