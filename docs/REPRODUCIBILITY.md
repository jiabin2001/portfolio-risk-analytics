# Reproducing the research

[Project overview](../README.md) · [Results](RESULTS.md) ·
[Methodology](../vignettes/methodology.qmd)

The repository includes executable R code, configuration profiles, an environment
lock and the tables behind the published findings. The full experiment can be run
from the command line or through `targets`.

## Environment

The recorded environment uses **R 4.5.1** and **Quarto 1.9.38**. The package declares
R >= 4.2, while the full dependency lock includes packages requiring R >= 4.4.
R 4.5.1 is the reference environment for reproducing the study.

Core modelling dependencies are `rugarch`, `VineCopula` and `quantmod`. The full
software environment is recorded in [renv.lock](../renv.lock). Restore it from the
repository root:

```sh
Rscript -e "install.packages('renv'); renv::restore()"
```

Network access is needed to install packages and to acquire market data when a
matching local cache is unavailable. Some platforms may need a compiler toolchain
and system libraries for packages without suitable binaries. The
[CI workflow](../.github/workflows/R-CMD-check.yaml) records the Linux setup.

Install Quarto on `PATH`, or on Windows place a portable build under `tools/`.
Windows rendering uses a project-local Quarto cache to support projects stored on a
different volume from the user profile.

## Inspect the published results

No model fitting or package installation is needed to read the committed CSV tables
and PNG figures. Check that they match their run manifest using base R:

```sh
Rscript --vanilla scripts/audit_outputs.R
```

The audit verifies file paths, existence, byte counts, MD5 hashes and agreement
between manifest and provenance. Generated CSV bytes are preserved in Git across
operating systems. Checksums establish file consistency, not statistical validity.

To render the report from these results after restoring the environment:

```sh
Rscript scripts/render_report.R
```

The output is `report/portfolio_risk_analytics.html`, with assets in the adjacent
`portfolio_risk_analytics_files/` directory. Keep both together when sharing the
rendered report. The report checks the manifest before reading its inputs.

## Run an experiment

| Profile | Purpose | Input | Evaluation |
| --- | --- | --- | --- |
| [Smoke](../config/smoke.yml) | Offline execution check | Synthetic fixture | 40 origins, four baseline models |
| [Validation](../config/validation.yml) | Smaller market-data run | Yahoo Finance | 156 origins, five baseline models |
| [Full](../config/full.yml) | Published research design | Yahoo Finance | 520 origins, 11 models including copula experiments |

Run the full experiment:

```sh
Rscript scripts/run_full_analysis.R
```

For an offline smoke run or a smaller market-data run:

```sh
Rscript scripts/run_smoke_test.R
Rscript scripts/run_validation.R
```

Each runner writes the active results to `outputs/tables/` and `outputs/figures/`.
Completed runs retain separate local archives. Run the full profile last to leave
the active outputs aligned with the published research design. Synthetic results
verify execution and are labelled separately from market-data evidence.

The recorded full experiment took approximately 24 minutes, excluding report
rendering, on its Windows host. Runtime varies with hardware, available model
engines, cached fits and checkpoint reuse.

## Data and numerical identity

The full profile requests daily adjusted index prices for `^FTSE` and `^GSPC` from
1 January 2000 through 31 December 2025. Weekly valuations use the latest quote
available by each Friday cutoff, with a maximum age of four calendar days. The
[valuation audit](../outputs/tables/valuation_audit.csv) includes prices, actual
quote dates and quote ages.

The study uses local-index returns. The configured base-currency label does not
apply an FX conversion in this mode. The analytical currency helper can accept
aligned FX returns, but these runners do not download them.

Provider histories can change. Restoring `renv.lock` fixes software dependencies;
it does not fix a later data download. Exact comparisons require the same input
snapshot, configuration, numerical engines and simulation settings.

Every completed run archives the following under `outputs/runs/<run_id>/`:

- Acquired price snapshot and experiment YAML.
- R session information and software versions.
- Generated tables, figures and their manifest.

These local archives, raw downloads, fitted model objects and checkpoints are
excluded from Git. Public research tables include the weekly valuation audit and
per-date forecasts. The provenance table identifies the numerical source revision,
which can precede subsequent documentation or report-presentation commits.

The simulation seed, draw count, chunk size and RNG kind are recorded. Changing
chunk size may change exact Monte Carlo draws even with the same seed. Cache and
checkpoint identities include inputs, settings and implementation details;
incompatible computations are recomputed.

## Pipeline execution

The `targets` workflow tracks configuration, input identity, generated files and
report source. Its default profile is the synthetic smoke experiment. Select the
full profile explicitly in R:

```r
Sys.setenv(PRA_CONFIG = "config/full.yml")
targets::tar_make()
```

Changing a tracked input or deleting a generated output invalidates the relevant
target. For a focused report-text edit, `scripts/render_report.R` renders the
current verified tables without refitting models.

## Numerical and software checks

Run the network-free test suite:

```sh
Rscript scripts/run_tests.R
```

The recorded validation passed 340 expectations with native engines and with the
full modelling dependencies. Coverage includes analytical risk identities,
empirical tail integration, date alignment, forecast timing, missing observations,
random-state isolation and checkpoint recovery. Native engines support a smaller
model set and are not numerically interchangeable with production dependencies.

The package also passed `R CMD check --no-manual --no-vignettes` on Windows, including
installed-package tests. To repeat a package check, use `R CMD build .`, then pass
the resulting archive filename to `R CMD check --no-manual --no-vignettes`.
GitHub Actions runs artifact auditing, dependency restoration, tests, package checks
and lint on Linux.

Software checks assess implementation. The empirical comparisons in
[Results](RESULTS.md) assess forecasting behaviour on market data; both are parts of
the research record.
