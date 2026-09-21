# Contributing

Contributions should make the statistical assumptions, implementation and evidence
easier to inspect and reproduce. Keep each pull request focused on a concrete problem
and describe the resulting behaviour. For a new model or estimator, explain its
intended use, mathematical definition and comparison baseline before expanding the
implementation.

## Development environment

Use R 4.5.1 for the locked environment. Run commands from the repository root. The
artifact audit uses base R and verifies the committed results without restoring
packages, downloading prices or fitting models:

```sh
Rscript --vanilla scripts/audit_outputs.R
Rscript -e "renv::restore(prompt = FALSE)"
Rscript scripts/run_tests.R
Rscript scripts/run_smoke_test.R
```

The tests and smoke profile use synthetic fixtures. They establish software behaviour,
not forecasting performance on market data. Add targeted tests for changed behaviour:
use analytical or hand-calculated numerical references where possible, and cover
failure cases, missing observations and time ordering when relevant. A successful
fit, finite output or passing test suite does not establish empirical model validity.
Keep public function documentation, generated Rd files and `NAMESPACE` consistent.
CI also runs package checks and linting.

## Numerical conventions

Preserve these conventions when changing the data or modelling layers:

- Returns are signed; negative returns represent losses. `loss_var` and `loss_es`
  retain signed loss-scale estimates for inference, including negative values when
  the relevant tail still earns a gain. `var` and `es` are display fields floored at
  zero. Do not substitute these display values in scoring or exceedance tests.
- Weekly observations use shared Friday valuation cutoffs, each asset's latest
  available quote within the configured same-week staleness limit, and retained
  source quote dates. Never use future quotes or silently collapse missing weeks.
- Portfolio weights apply in simple-return space before conversion to the configured
  return space. The default FTSE/S&P 500 example combines local-currency index
  returns; an investable base-currency result requires explicit FX treatment.
- Forecasts at an origin may use only information available through that origin.
  Comparative losses use common dates, while coverage and availability retain each
  model's full calendar. Missing forecasts remain gaps in transition tests. FZ0
  scoring requires positive signed loss ES; report unscorable observations explicitly.

## Empirical contributions

Changes to empirical results need a separate, reproducible experiment. Record the
data period and source, forecast horizon, training and evaluation windows, model
specifications, benchmarks, seeds, simulation budget and scoring rules. Fix these
choices before examining the evaluation results. Do not change seeds, windows,
model sets or bootstrap settings to obtain significance. If sensitivity analysis or
exploratory tuning is necessary, report the alternatives and their selection process;
use a separate evaluation sample for subsequent confirmatory claims.

The real-data entry points are:

```sh
Rscript scripts/run_validation.R
Rscript scripts/run_full_analysis.R
```

These runs require market-data access or a matching local cache. The full profile
requires the locked production model packages and Quarto for its HTML report. Compare
predictive losses with uncertainty and availability, alongside runtime. Report null
or inconclusive findings, failed fits and limitations; a loss rank or a non-rejected
coverage test alone is not evidence of superiority. See the
[results](docs/RESULTS.md), [reproducibility guide](docs/REPRODUCIBILITY.md) and
[methodology](vignettes/methodology.qmd) for the experiment and its interpretation.

## Publishing artifacts

Smoke and research runners both replace the current files under `outputs/`. Review
those changes separately from code changes. If a pull request intentionally publishes
new results, include a coherent artifact set with its manifest and provenance, then
rerun the artifact audit. Preserve the configured CSV byte handling so checksums
survive checkout. Raw downloads, binary snapshots, fitted model objects, checkpoints
and caches remain local; do not include credentials, secrets or private data.

## Pull requests

In the pull request, state what changed and why, which checks or experiments ran,
and any remaining limits. Distinguish verified numerical behaviour from conclusions
supported by the evaluation sample. Update the lockfile only when dependency changes
are intentional, and explain their effect on reproducibility.
