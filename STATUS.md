# Project Status

Last updated: 2026-07-18 12:26 Europe/London

## Completed

- Independent package-style implementation with configuration, data, returns, baseline,
  marginal, PIT, copula, simulation, rolling, backtesting, comparison, plotting, and
  reporting modules.
- Project-local `renv` library/cache/sandbox configuration and consistent `renv.lock`.
- Raw, processed, and dependency-aware cache artefacts kept in separate project directories.
- Synthetic fixture smoke run plus real-data validation and full-profile runs.
- Roxygen documentation and generated `NAMESPACE`/`man/` pages.
- GitHub Actions workflow, MIT licence, contribution guide, Quarto report, and README.
- Portable Quarto 1.9.38 verified by SHA-256 and kept under ignored `tools/`; HTML report rendered.

## Tested

- Network-free test suite: **73 expectations passed, 0 failed, 0 errors**, repeated after
  the full-profile output and documentation refresh.
- Final standard `R CMD check --no-manual`: **Status: OK** (0 errors, 0 warnings, 0 notes).
- Installed-package tests also pass inside `R CMD check`.
- GitHub Actions run `29640689344` passed dependency restore, unit tests,
  `R CMD check --as-cran`, and lint on Ubuntu with R 4.5.1.
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

Completed successfully as run `full_20260718T102619` in **4,433.910 seconds**. The run
used 1,338 aligned weekly observations from 2000-01-07 through 2025-12-30 and local-index-
return mode with no FX conversion.

- Marginal grid: **1,080/1,080 converged** and **648** passed every configured diagnostic.
- Selected FTSE marginal: `ARMA(0,0)-eGARCH(1,1)-sstd`.
- Selected S&P 500 marginal: `ARMA(0,1)-eGARCH(2,1)-sstd`.
- Copulas: 6/6 fitted; BIC selected BB1 with Kendall tau **0.485844**,
  lower-tail dependence **0.544908**, and upper-tail dependence **0.343948**.
- 100,000-draw risk: 95% VaR **0.022392**, 95% ES **0.031407**;
  99% VaR **0.036787**, 99% ES **0.045767**.
- Monte Carlo convergence: seven sample sizes from 1,000 to 100,000, five seeds, and two
  confidence levels produced 70 detail rows and 14 summary rows.
- Rolling evaluation: 260 forecast origins, six models, two confidence levels, and
  **3,120/3,120 successful forecast rows** with zero model failures.
- Composite rank at 95%: copula-GARCH first, 10 exceptions (3.846%), conditional-coverage
  p-value 0.4510, mean quantile loss 0.002101, and green coverage status.
- Composite rank at 99%: Gaussian first, 3 exceptions (1.154%), conditional-coverage
  p-value 0.0620, mean quantile loss 0.000668, and green coverage status.
- The dependency-fingerprint caches and rolling checkpoints completed without restart.

## Failed or skipped models

- Validation: zero convergence failures; 168 converged candidates failed one or more
  diagnostic validity constraints and remain visible with rejection reasons.
- Full: zero convergence failures; 432 converged candidates failed one or more diagnostic
  validity constraints and remain visible with rejection reasons.
- No copula candidate failed in validation.
- No copula or rolling forecast candidate failed in the full profile.
- Native fallback supports only normal/Student-t sGARCH/eGARCH(1,1); other native
  candidates are structured failures when production packages are unavailable.

## Known warnings

- Weekly `xts` aggregation reports that missing source observations were removed; final
  common-date alignment and sample bounds are validated.
- `testthat` 3.3.2 was built under R 4.5.3 while the runtime is R 4.5.1; all tests pass.
- The first `--as-cran` check could not perform CRAN incoming network checks in the
  sandbox. The subsequent standard package check completed with `Status: OK`.
- GitHub Actions passes but reports a non-blocking Node 20 deprecation annotation for
  `actions/checkout@v4` and non-blocking configured style-lint annotations.

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

- Six full-profile PNG figures under `outputs/figures/`, including Monte Carlo convergence.
- Nine full-profile CSV tables under `outputs/tables/`, including convergence detail and
  summary, rolling forecasts, model comparison, and the output manifest.
- `report/portfolio_risk_analytics.html` rendered from generated outputs (51,617 bytes).
- Structured logs under ignored `outputs/logs/`, models under ignored `outputs/models/`,
  and resumable checkpoints under ignored `outputs/checkpoints/`.

## Remaining tasks

- No required implementation or verification task remains.
- Optionally reduce the remaining non-functional style lints and upgrade
  `actions/checkout` after confirming the preferred Node 24-compatible major version.

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
