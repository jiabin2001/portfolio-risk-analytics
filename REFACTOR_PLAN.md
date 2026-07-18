# Refactor Plan

## 1. Initial repository audit

The working directory started without a Git history or public project files. Excluded
audit inputs are not runtime dependencies. The public implementation is therefore an
independent, package-style build.

## 2. Initial state

- No R package metadata, tests, pipeline, documentation, or generated results existed.
- R 4.5.1 is installed, but the project initially had no third-party R packages.
- Quarto is not currently installed, so HTML report rendering is an optional final step.
- No market-data cache existed at audit time.

## 3. Method flow identified for independent implementation

The analytical flow to preserve at a conceptual level is: acquire and align prices;
construct returns; estimate conditional marginal distributions; transform residuals
with the probability integral transform (PIT); estimate cross-asset dependence; draw
joint scenarios; restore conditional means and volatilities; aggregate simple returns;
and estimate portfolio tail risk.

The new system extends this into a genuine out-of-sample framework with benchmark
models, rolling forecasts, VaR and ES evaluation, model-failure accounting, and
reproducible outputs.

## 4. Statistical issues to avoid or correct

- Never describe VaR as a maximum loss. VaR and ES are positive loss magnitudes.
- Construct portfolio simple returns before taking `log1p`; do not average asset log
  returns and call the result an exact portfolio log return.
- Keep model diagnostics separate from information-criterion ranking. Do not maximize
  a diagnostic p-value as a selection rule.
- Clip PIT values away from zero and one before inverse-CDF operations, recording the
  number clipped.
- Interpret copula tails on the return-CDF scale: lower-tail dependence represents
  joint extreme negative returns; upper-tail dependence is not crash dependence.
- Use only information available at each forecast origin and align the realised return
  to the next forecast date.
- Treat zero/all exceptions and zero transition cells explicitly in likelihood-ratio
  tests rather than relying on undefined logarithms.
- Distinguish descriptive ES evaluation from formal ES backtests.
- State the currency mode explicitly. Local-index-return mode performs no FX conversion;
  base-currency mode requires aligned FX returns.

## 5. Software-engineering issues to avoid or correct

- No monolithic scripts, hard-coded absolute paths, hidden global state, silent errors,
  manually copied results, or live-network tests.
- All failures return structured status fields and are written to project-local logs.
- Configuration controls data dates, model grids, windows, seeds, confidence levels,
  simulation budgets, caching, and workers.
- Raw data, processed data, caches, checkpoints, models, logs, and outputs remain under
  the repository root.

## 6. Target architecture

- An R package in `R/`, documented with roxygen comments and exported through
  `NAMESPACE`.
- Three YAML profiles under `config/`: smoke, validation, and full.
- Thin executable entry points under `scripts/`.
- Network-free unit and integration tests under `tests/testthat/`.
- A dependency-aware `_targets.R` pipeline plus direct runner scripts for environments
  where `targets` is unavailable.
- Generated tables, figures, logs, and checkpoints under `outputs/`.
- A Quarto report that reads generated artefacts rather than embedding model results.

## 7. Data and model flow

1. Read and validate configuration.
2. Create project-local directories and a structured run log.
3. Load cached/local prices or download adjusted prices once and record metadata.
4. Validate and align dates; calculate asset simple and log returns.
5. Apply the configured currency mode and calculate portfolio returns.
6. Produce historical, Gaussian, Student-t, EWMA, and filtered-historical forecasts.
7. Fit a configuration-driven marginal-model grid and retain every success/failure.
8. Run residual, squared-residual, and PIT diagnostics; select by validity then IC.
9. Fit candidate copulas, report tail metadata, and choose a valid IC-ranked model.
10. Simulate joint returns in chunks, aggregate in simple-return space, and compute VaR/ES.
11. Run leakage-free rolling one-step forecasts with resumable checkpoints.
12. Backtest, compare models, plot results, and persist machine-readable artefacts.

## 8. Package choices

- Base `stats`, `utils`, `graphics`, and `grDevices` for core calculations and stable
  fallbacks.
- `yaml` for configuration and `testthat` for tests.
- `ggplot2` for publication-quality figures.
- `rugarch` for production ARMA-GARCH/eGARCH fits when available; a transparent EWMA/
  ARMA fallback keeps the software testable without disguising the fallback as GARCH.
- `VineCopula` for rotated and Archimedean family estimation when available; native
  Gaussian and Student-t copula fallbacks cover deterministic smoke tests.
- `quantmod` for online data acquisition, always behind the project cache.
- `targets` for dependency-aware orchestration and `renv` for project-local dependency
  capture.

Optional dependencies never fail silently: capability checks, fallback names, warnings,
and skipped-model tables are part of the outputs.

## 9. Testing strategy

Tests cover date/price validation, return identities, weights and currency modes, VaR/ES
sign conventions, analytical benchmarks, PIT boundaries, deterministic simulation,
structured model failures, Kupiec/Christoffersen edge cases, no-look-ahead rolling
indices, configuration parsing, directory creation, and a fixture-based smoke run.
Tests do not require a network connection.

## 10. Backtesting strategy

Generate one-step-ahead forecasts with moving or expanding windows. At origin `t`, fit
only observations through `t` and score against `t + 1`. Report exception counts and
rates, Kupiec unconditional coverage, Christoffersen independence and conditional
coverage, pinball loss, descriptive ES residuals, a joint VaR/ES score, and traffic-light
labels. Rank models using calibration, independence, loss, stability, complexity, and
runtime rather than one p-value.

## 11. Performance strategy

- Cache downloads, processed data, and reusable model fits.
- Checkpoint rolling forecasts atomically and resume by forecast key.
- Simulate in configurable chunks and retain only requested scenarios.
- Use sequential execution by default. If enabled, use no more than half the logical
  cores while reserving at least two cores.
- Record stage runtimes, simulation counts, failed fits, and cache hits.

## 12. Full-run strategy

First prove the fixture-based smoke profile, then use live or cached market data for the
validation profile. Estimate the full-profile runtime from validation, verify resume
behaviour, and run the long history with 100,000 draws for the current-risk estimate and
a smaller configured budget per rolling origin. The convergence study is a separate,
resumable stage.

## 13. Risks and fallbacks

- **No network/data source:** use an existing cache; otherwise fail the real-data stage
  clearly while retaining fixture-only software validation.
- **Optional package unavailable:** record skipped capabilities and use named, statistically
  simpler fallbacks where valid.
- **Marginal convergence failure:** retain the failed result and select the best valid
  candidate; if none is valid, use the configured EWMA/empirical fallback with warning.
- **Copula failure:** fall back to a Gaussian copula estimated from clipped normal scores,
  with explicit metadata.
- **Quarto unavailable:** generate all report inputs and leave an exact render command;
  HTML rendering remains pending rather than being claimed successful.
- **Long runtime:** checkpoint, report an estimate, and resume rather than discarding work.

## 14. Definition of done

Done means that the package structure and profiles are complete; core data, returns,
risk, marginal, copula, simulation, rolling, and backtesting functions run; unit tests
and the smoke run pass; real-data validation and full runs either complete or have an
honest resumable status; generated outputs feed the report and README; R CMD check has
no critical error; no prohibited source information or fabricated result is present; excluded
materials remain ignored; and all remaining limitations are explicit in `STATUS.md`.
