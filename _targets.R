source("scripts/bootstrap.R")

if (!requireNamespace("targets", quietly = TRUE)) stop("The optional `targets` package is required for this pipeline.")
library(targets)

tar_option_set(packages = character(), format = "rds", error = "stop")

list(
  tar_target(config_file, Sys.getenv("PRA_CONFIG", "config/smoke.yml"), format = "file"),
  tar_target(config, read_config(config_file)),
  tar_target(analysis, run_analysis(config$config_path)),
  tar_target(files, analysis$files),
  tar_target(report, {
    if (isTRUE(config$report$render) && nzchar(Sys.which("quarto"))) {
      status <- system2(Sys.which("quarto"), c("render", "report/portfolio_risk_analytics.qmd", "--to", "html"))
      if (!identical(status, 0L)) stop("Quarto rendering failed.", call. = FALSE)
      project_path("report", "portfolio_risk_analytics.html")
    } else "report_inputs_ready"
  })
)
