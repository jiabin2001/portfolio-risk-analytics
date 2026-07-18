script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_arg)) dirname(normalizePath(sub("^--file=", "", script_arg[[1]]))) else "scripts"
source(file.path(script_dir, "bootstrap.R"))
setwd(.PRA_PROJECT_ROOT)

quarto <- Sys.which("quarto")
if (!nzchar(quarto)) {
  local_candidates <- list.files("tools", pattern = "quarto.exe$", recursive = TRUE, full.names = TRUE)
  if (length(local_candidates)) quarto <- normalizePath(local_candidates[[1]], winslash = "/")
}
if (!nzchar(quarto)) stop("Quarto is not installed or not on PATH. Analysis outputs are intact; install Quarto and rerun.", call. = FALSE)
Sys.setenv(PRA_QUARTO_USER_DIR = project_path("tools", "quarto-user"))
status <- system2(quarto, c("render", "report/portfolio_risk_analytics.qmd", "--to", "html"))
if (!identical(status, 0L)) stop("Quarto rendering failed.", call. = FALSE)
cat("Rendered report/portfolio_risk_analytics.html\n")
