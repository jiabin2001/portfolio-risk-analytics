# Project Status

Last updated: 2026-07-18 10:27 Europe/London

## Completed

- Independent package-style implementation with configuration, data, returns, baseline,
  marginal, PIT, copula, simulation, rolling, backtesting, comparison, plotting, and
  reporting modules.
- Project-local `renv` library/cache/sandbox configuration and consistent `renv.lock`.
- Raw, processed, and dependency-aware cache artefacts kept in separate project directories.
- Synthetic fixture smoke run and real-data validation run.
- Roxygen documentation and generated `NAMESPACE`/`man/` pages.
- GitHub Actions workflow, MIT licence, contribution guide, Quarto report, and README.
- Portable Quarto 1.9.38 verified by SHA-256 and kept under ignored `tools/`; HTML report rendered.

## Tested

- Network-free test suite: **73 expectations passed, 0 failed, 0 errors**.
- Standard `R CMD check --no-manual`: **Status: OK** (0 errors, 0 warnings, 0 notes).
- Installed-package tests also pass inside `R CMD check`.
- Copula-GARCH checkpoint audit: first run wrote four rows for two origins; resumed run
  left the checkpoint timestamp unchanged and all first-run rows had `status = ok`.
- Configured lintr: 121 non-functional style findings (36 long lines, 34 semicolons,
  32 indentation, 17 brace, 2 object-length); no correctness issue was reported by
  `R CMD check` code analysis.

## Smoke results

- Profile: explicitly synthetic two-asset fixture; not real market data.
- Runtime: **6.075 seconds**.
- Marginal engine: `rugarch`; copula candidates through `VineCopula`.
- Selected copula: Gumbel.
- 2,000-draw current risk: 95% VaR **0.019064**, 95% ES **0.024536**;
  99% VaR **0.026757**, 99% ES **0.033644**.
- Rolling evaluation: 40 origins, four benchmark models, two confidence levels
  (320 common-schema forecast rows).

## Validation results

- Data: 1,082 aligned weekly observations, 2005-01-07 through 2025-12-30, cached from
  Yahoo Finance; local-index-return mode with no FX conversion.
- Runtime: **147.676 seconds**.
- Marginal grid: **288/288 converged**; **120** passed every configured diagnostic.
- Selected FTSE marginal: `ARMA(0,0)-eGARCH(2,1)-sstd`.
- Selected S&P 500 marginal: `ARMA(0,1)-eGARCH(2,1)-sstd`.
- Copulas: 6/6 fitted; BIC selected BB1 with Kendall tau **0.478591**,
  lower-tail dependence **0.557949**, and upper-tail dependence **0.311974**.
- 20,000-draw risk: 95% VaR **0.021897**, 95% ES **0.030627**;
  99% VaR **0.035846**, 99% ES **0.044023**.
- Rolling benchmark evaluation: 156 origins and 1,560 successful forecast rows.
- Composite rank at 95%: Student-t first, 3 exceptions (1.923%), conditional-coverage
  p-value 0.1262, mean quantile loss 0.001998; coverage traffic light is amber because
  the exception rate is materially below the expected 5%.
- Composite rank at 99%: Gaussian first, 1 exception (0.641%), conditional-coverage
  p-value 0.8844, mean quantile loss 0.000734; coverage traffic light is green.

## Full-run results

Running; no full-profile risk result is reported yet. The checkpointed run started at
2026-07-18 10:26 Europe/London in managed execution cell `166`. Its structured log is
`outputs/logs/full_20260718T102619.tsv`; the first two entries confirm the full profile
started and loaded the cached aligned prices in 0.030 seconds. It is currently in the
initial marginal-grid stage, before the first rolling checkpoint is expected.

The full data cache contains 1,338 aligned weekly observations from 2000-01-07 through
2025-12-30. An 18-candidate stratified timing sample took 22.75 seconds with 18/18
convergence. With a 540-candidate grid per asset, 100,000 current draws, four periodic
model reselections, 260 copula-GARCH forecast origins, and convergence analysis, the
current estimate is approximately **2.0-2.5 hours**. The long run is checkpointed by
forecast date and the current marginal/copula stage is dependency-fingerprint cached.

## Failed or skipped models

- Validation: zero convergence failures; 168 converged candidates failed one or more
  diagnostic validity constraints and remain visible with rejection reasons.
- No copula candidate failed in validation.
- Native fallback supports only normal/Student-t sGARCH/eGARCH(1,1); other native
  candidates are structured failures when production packages are unavailable.

## Known warnings

- Weekly `xts` aggregation reports that missing source observations were removed; final
  common-date alignment and sample bounds are validated.
- `testthat` 3.3.2 was built under R 4.5.3 while the runtime is R 4.5.1; all tests pass.
- The first `--as-cran` check could not perform CRAN incoming network checks in the
  sandbox. The subsequent standard package check completed with `Status: OK`.

## Known limitations

- The validated cross-index example is in local-index-return mode and is not an
  FX-adjusted investable base-currency portfolio.
- Copula simulation is currently bivariate, although data and return functions accept
  arbitrary asset counts.
- Expected Shortfall evaluation is descriptive plus a joint VaR/ES scoring rule; it is
  not presented as a formal ES hypothesis test.
- Rolling copula-GARCH refits each selected specification at every origin for statistical
  integrity; this is deliberately compute intensive.
- Periodic rebalancing and transaction costs are interfaces for future work, not current
  production features.

## Runtime and dependencies

R 4.5.1. Principal versions: `rugarch` 1.5-5, `VineCopula` 2.6.1, `quantmod`
0.4-29, `targets` 1.12.0, `testthat` 3.3.2, `yaml` 2.3.12, `renv` 1.2.3,
`R.utils` 2.13.0, and `rmarkdown` 2.31. Execution is sequential by default; workers are
bounded by configuration and no GPU path is used.

## Outputs generated

- Five validation PNG figures under `outputs/figures/`.
- Current baseline/copula risk, marginal selection, copula selection, rolling forecasts,
  model comparison, and manifest CSV files under `outputs/tables/`.
- `report/portfolio_risk_analytics.html` rendered from generated outputs (49,232 bytes).
- Structured logs under ignored `outputs/logs/`, models under ignored `outputs/models/`,
  and resumable checkpoints under ignored `outputs/checkpoints/`.

## Remaining tasks

- Monitor the active checkpointed full run through completion.
- On completion, refresh report/README values from full outputs and rerun final check.
- Optionally reduce the remaining non-functional style lints.

## Exact reproduction commands

```powershell
Rscript -e "renv::restore()"
Rscript scripts/run_tests.R
Rscript scripts/run_smoke_test.R
Rscript scripts/run_validation.R
Rscript scripts/run_full_analysis.R
Rscript scripts/render_report.R
R CMD build .
R CMD check --no-manual portfoliorisk_0.1.0.tar.gz
```
