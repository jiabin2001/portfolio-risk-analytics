source("scripts/bootstrap.R")

started <- Sys.time()
result <- run_analysis("config/smoke.yml", run_copula_rolling = FALSE)
cat(sprintf("Smoke run completed: %s\n", result$run_id))
cat(sprintf("Elapsed seconds: %.3f\n", as.numeric(difftime(Sys.time(), started, units = "secs"))))
cat(sprintf("Selected copula: %s\n", result$copula_selection$selected$family))
print(result$simulation$risk)
print(result$comparison)
