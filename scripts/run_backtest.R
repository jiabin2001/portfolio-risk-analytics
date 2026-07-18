source("scripts/bootstrap.R")

args <- commandArgs(trailingOnly = TRUE)
profile <- if (length(args)) args[[1]] else "smoke"
cfg <- read_config(file.path("config", paste0(profile, ".yml")))
result <- run_analysis(cfg$config_path, run_copula_rolling = "copula_garch" %in% cfg$rolling$models)
print(result$comparison)
