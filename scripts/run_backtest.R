script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_arg)) dirname(normalizePath(sub("^--file=", "", script_arg[[1]]))) else "scripts"
source(file.path(script_dir, "bootstrap.R"))
setwd(.PRA_PROJECT_ROOT)

args <- commandArgs(trailingOnly = TRUE)
profile <- if (length(args)) args[[1]] else "smoke"
cfg <- read_config(file.path("config", paste0(profile, ".yml")))
result <- run_analysis(cfg$config_path, run_copula_rolling = "copula_garch" %in% cfg$rolling$models)
print(result$comparison)
