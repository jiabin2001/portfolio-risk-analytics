source("scripts/bootstrap.R")

if (!requireNamespace("targets", quietly = TRUE)) stop("The optional `targets` package is required for this pipeline.")
library(targets)

tar_option_set(packages = character(), format = "rds", error = "stop")

list(
  tar_target(config_file, Sys.getenv("PRA_CONFIG", "config/smoke.yml"), format = "file"),
  tar_target(config, read_config(config_file)),
  tar_target(input_state, list(implementation = implementation_fingerprint(),
    request = data_request_signature(config, pra_project_root())), cue = tar_cue(mode = "always")),
  tar_target(files, {
    input_state
    unname(run_analysis(config$config_path)$files)
  }, format = "file"),
  tar_target(report_source, "report/portfolio_risk_analytics.qmd", format = "file"),
  tar_target(report, {
    stopifnot(all(file.exists(files)), file.exists(report_source))
    if (isTRUE(config$report$render)) render_analysis_report(report_source)
    else files[grepl("output_manifest[.]csv$", files)]
  }, format = "file")
)
