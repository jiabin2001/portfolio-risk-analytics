# Project Status

Updated: 2026-09-21 (Asia/Shanghai). Version 0.2.0.

## Verified implementation

- Calendar-safe Friday valuations retain holiday weeks and expose actual quote dates.
- Exact empirical ES tail mass, signed risk estimates and explicit FZ0 domain accounting.
- Common-date scoring, gap-preserving coverage tests and failure-aware checkpoints.
- Portfolio GARCH-t benchmark, four shared-marginal copula ablations, and paired block-bootstrap inference.
- Frozen local input archives, source/configuration identifiers and checksummed public outputs.
- Base-R artifact auditing runs in CI; CSV byte preservation keeps hashes portable.

## Validation

- **340 expectations passed** with native engines and with production dependencies.
- **R CMD check --no-manual --no-vignettes: Status OK**, with no errors, warnings or notes.
- Installed-package tests passed as part of R CMD check.
- Production smoke pipeline passed; 40 origins and four benchmarks produced 320 rows.
- Deleting a generated figure and rerunning targets regenerated it through its producing target.
- A copula-only simple-return run with independence/Gaussian ablations passed; disabling every model fails early.
- Corrupt output checksums, changed byte counts, path traversal and mixed run identifiers are rejected by the artifact audit.
- The full real-data run and HTML report below completed successfully.

The package check used R 4.5.1 on Windows. See GitHub Actions for the independently
executed Linux check; local results are not a claim about an unobserved CI run.

## Full experiment

Run: `full_20260920T230325.621650_26556`.

Model source: `3ccf0d4abd4375152f614cd2a94a2a7b68c795b8` (clean model source at run start).
Later publication changes update documentation, report rendering, entry-point
checks and publication/CI; they do not change fitted models or scores.

- Pipeline elapsed wall time: **1455 seconds** (excluding report rendering).
- **1356** Friday valuations, 2000-01-07 through 2025-12-26; all return intervals are seven days.
- **83** quotes predate their Friday cutoff; maximum quote age is **4** calendar days.
- Equal-weight FTSE/S&P 500 local-index log returns, without FX conversion or transaction costs.
- Moving estimation window: 520 observations; specification reselection every 65 origins.
- Out-of-sample dates: **2016-01-15 through 2025-12-26**, across **520 origins**.
- **11 models x 2 confidence levels = 11,440 forecast rows; 0 failures.**
- Current simulation: 100,000 draws; rolling simulation: 10,000 draws per model/origin.
- Comparative inference: 1,999 paired circular-block resamples, 13-week blocks, GARCH-t reference.
- Current marginal grid: 48 candidates, 48 converged, 42 passed diagnostic screening.
- Current copula grid: 6 fitted candidates; BIC selected **BB1**.

Selected current marginal specifications:

- `^FTSE`: `arma00_egarch11_sstd`.
- `^GSPC`: `arma01_egarch11_sstd`.

## Comparative evidence

Differences below are copula-GARCH minus portfolio GARCH-t; lower is better.
All comparisons use the same 520 dates. Intervals are marginal 95% percentile
intervals; Holm p-values adjust the 40 comparisons in this run.

| Forecast confidence | Score | Mean difference | 95% interval | Holm p-value |
| --- | --- | ---: | --- | ---: |
| 95% | quantile_loss | -5.1347e-05 | [-0.00024269, 0.00010879] |      1 |
| 95% | joint_var_es | -0.059839 | [-0.20648, 0.057643] |      1 |
| 99% | quantile_loss | -5.5452e-05 | [-0.00017518, 4.6186e-05] |      1 |
| 99% | joint_var_es | -0.16564 | [-0.4793, 0.084433] |      1 |

All four intervals cross zero and none of these comparisons is significant after
Holm adjustment. Observed copula losses are lower, but this sample does not establish
superiority over GARCH-t. Ranks are descriptive; a large coverage-test p-value does
not prove calibration, and an interval crossing zero does not establish equivalence.

The full benchmark, copula-ablation, coverage and loss tables are committed under
`outputs/tables/`. The report also displays uncertainty intervals and convergence.

## Current risk snapshot

Conditional on the last valuation date; these estimates are separate from historical
out-of-sample validation. Values are log-return loss units.

| Confidence | VaR | ES |
| --- | ---: | ---: |
| 95% | 0.023773 | 0.033751 |
| 99% | 0.039798 | 0.050228 |

## Reproducibility and limits

- Input hash: `393ba7a66ae763f552ba3c4aa4318c4e`.
- Aligned-price hash: `a257a5edae9f0e9be918a718df8c30f9`.
- Configuration hash: `f5490b2ab328a0b391e19c6e7009ba1e`.
- Implementation hash: `e1cd44936a463048ea6ae800572d2411`.
- Software: R 4.5.1, rugarch 1.5.6, VineCopula 2.6.1, quantmod 0.4.29; see renv.lock for the full environment.
- Quarto 1.9.38 rendered the HTML report from hash-verified outputs.
- Local archives retain the frozen inputs, configuration, session information and complete generated outputs.
- Public artifacts include weekly valuation prices and quote dates; full raw downloads and binary snapshots remain local.
- Re-downloading historical prices may change results. The software lock does not freeze provider data.

This is a bivariate research framework. It omits FX acquisition, a holdings ledger,
execution costs and operational production controls. Residual screening and
fitted-sample PIT diagnostics are not proofs of correct specification. The bootstrap
assumes sufficiently stable loss differences and does not adjust for tuning on the
holdout. Even 520 weeks offer limited information about rare 99% exceptions.

**The v0.1 output and composite-ranking claims are superseded** because their
exact-date join omitted holiday weeks and produced inconsistent return horizons.

## Reproduction

Use R 4.5.1. Install Quarto, or keep a portable executable under `tools/`, for HTML.

```sh
Rscript --vanilla scripts/audit_outputs.R
Rscript -e "renv::restore()"
Rscript scripts/run_tests.R
Rscript scripts/run_smoke_test.R
Rscript scripts/run_full_analysis.R
R CMD build .
R CMD check --no-manual --no-vignettes portfoliorisk_0.2.0.tar.gz
```

Smoke/full runs replace the active output tables; each completed run retains its
own local archive. `scripts/render_report.R` rerenders the current verified outputs.
