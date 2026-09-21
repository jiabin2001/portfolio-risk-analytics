# Empirical findings

[Project overview](../README.md) · [Methodology](../vignettes/methodology.qmd) ·
[Reproduction](REPRODUCIBILITY.md)

This study evaluates one-week portfolio VaR and ES forecasts for an equal-weight
combination of FTSE 100 and S&P 500 local-index returns. Its central comparison is
between an asset-level copula-GARCH construction and a univariate portfolio GARCH-t
benchmark. Fixed-marginal copula experiments examine dependence separately.

## Sample and experiment

Daily Yahoo Finance prices produce 1,356 common Friday valuations from 7 January
2000 to 26 December 2025. Each valuation uses the most recent quote available by
its cutoff, within four calendar days. The audit contains 83 quotes dated before
their Friday cutoff; all return intervals are seven days.

The evaluation covers **520 forecast dates, 15 January 2016–26 December 2025**.
Every origin uses a moving 520-observation estimation window. Asset marginal and
copula specifications are reselected every 65 origins, with refitting at every
origin. All 11 models produce forecasts at both confidence levels, giving 11,440
successful forecast rows and the same 520 scoring dates for every comparison.

The portfolio uses fixed 50/50 weights in simple-return space before conversion to
log returns. It represents local-index risk; FX conversion and implementation costs
are outside the experiment.

## Forecast accuracy

The primary reference is a univariate portfolio sGARCH(1,1) model with Student-t
innovations. Quantile loss evaluates VaR, while the Fissler–Ziegel FZ0 score evaluates
VaR and ES jointly. Lower mean scores indicate better observed forecasts within the
same confidence level and scoring rule.

| Measure | GARCH-t | Copula-GARCH | Observed difference |
| --- | ---: | ---: | ---: |
| 95% quantile loss | 0.00242004 | 0.00236869 | 2.12% lower |
| 99% quantile loss | 0.00081528 | 0.00075983 | 6.80% lower |
| 95% joint VaR/ES score | −3.081251 | −3.141090 | −0.059839 |
| 99% joint VaR/ES score | −2.484146 | −2.649782 | −0.165637 |

These are improvements in forecast scoring, not investment returns or percentage
reductions in portfolio risk. Percentage changes are used for positive quantile
losses; joint-score differences are reported in score units.

The paired comparison uses 1,999 circular block-bootstrap resamples with 13-week
blocks. Differences below are copula-GARCH minus GARCH-t. The intervals use a nominal
95% confidence level and are marginal, not simultaneous. Holm adjustment covers all
40 comparisons in the experiment.

| Measure | Mean difference | 95% interval | Unadjusted p-value | Holm p-value |
| --- | ---: | --- | ---: | ---: |
| 95% quantile loss | −0.00005135 | [−0.00024269, 0.00010879] | 0.5565 | 1.0000 |
| 99% quantile loss | −0.00005545 | [−0.00017518, 0.00004619] | 0.3610 | 1.0000 |
| 95% joint VaR/ES score | −0.059839 | [−0.206476, 0.057643] | 0.3805 | 1.0000 |
| 99% joint VaR/ES score | −0.165637 | [−0.479295, 0.084433] | 0.2565 | 1.0000 |

The direction of the point estimates is consistent across the four comparisons.
Their intervals nevertheless include zero, so this sample leaves uncertainty about
the incremental predictive benefit over GARCH-t. The results support further study
of that benefit without establishing equivalence or a reliable advantage.

Sources: [model scores](../outputs/tables/model_comparison.csv),
[paired inference](../outputs/tables/predictive_comparison.csv), and
[per-date score differences](../outputs/tables/predictive_scores.csv).

## Calibration and dependence

VaR calibration concerns the frequency and temporal pattern of exceptions. At 95%
and 99% confidence, the expected counts over 520 observations are 26 and 5.2.

| Model | 95% exceptions | 99% exceptions |
| --- | ---: | ---: |
| Copula-GARCH | 23 (4.42%) | 4 (0.77%) |
| Portfolio GARCH-t | 37 (7.12%) | 8 (1.54%) |
| Independence, shared marginals | 43 (8.27%) | 15 (2.88%) |
| Gaussian copula, shared marginals | 24 (4.62%) | 5 (0.96%) |
| Student-t copula, shared marginals | 24 (4.62%) | 4 (0.77%) |
| BB1 copula, shared marginals | 23 (4.42%) | 4 (0.77%) |

The independence experiment holds the asset marginal forecasts fixed while removing
cross-asset dependence. Its higher exception rates illustrate the effect of that
assumption on this portfolio's forecast risk. Gaussian and Student-t dependence
models give exception frequencies closer to their nominal targets in this sample.

Copula-GARCH conditional-coverage p-values are 0.8274 and 0.8328 at the two levels.
These describe compatibility with the coverage model, rather than the probability
that a forecasting model is correct. Only four copula-GARCH exceptions occur at 99%,
which limits the precision of tail-calibration conclusions.

## Comparison across models

The table is ordered by modelling approach. It reports observed quantile losses;
statistical comparisons are available in the paired-inference table.

| Model | 95% quantile loss | 99% quantile loss |
| --- | ---: | ---: |
| Historical | 0.00269060 | 0.00102884 |
| Gaussian | 0.00269642 | 0.00104927 |
| Student-t | 0.00264743 | 0.00103495 |
| EWMA | 0.00254437 | 0.00100667 |
| Filtered historical | 0.00258719 | 0.00097314 |
| Portfolio GARCH-t | 0.00242004 | 0.00081528 |
| Copula-GARCH, selected family | 0.00236869 | 0.00075983 |
| Independence, shared marginals | 0.00241639 | 0.00082955 |
| Gaussian copula, shared marginals | 0.00236284 | 0.00075315 |
| Student-t copula, shared marginals | 0.00236138 | 0.00075403 |
| BB1 copula, shared marginals | 0.00237059 | 0.00076494 |

Gaussian and Student-t copulas achieve slightly lower observed quantile losses than
the selected-family model. BB1's asymmetric tails therefore do not, by themselves,
explain a forecasting advantage. The comparison with portfolio GARCH-t changes both
the marginal construction and the dependence representation; the fixed-marginal
experiments are the more direct comparison of dependence assumptions.

![Common-date model comparison](../outputs/figures/model_comparison.png)

## Simulation precision and terminal estimates

Rolling copula forecasts use 10,000 draws at each origin. The terminal risk estimate
uses 100,000 draws, with the fitted asset marginals `arma00_egarch11_sstd` for FTSE
and `arma01_egarch11_sstd` for S&P 500. BIC selects BB1 at the terminal date.
This terminal fit is separate from historical out-of-sample performance.

Conditional on the final valuation on 26 December 2025:

| Confidence | VaR | ES |
| --- | ---: | ---: |
| 95% | 0.023773 | 0.033751 |
| 99% | 0.039798 | 0.050228 |

Values are log-return loss units. The convergence experiment repeats seven draw
counts, from 1,000 to 100,000, across five seeds. Its variation measures simulation
precision conditional on the fitted model; parameter and model-selection
uncertainty are separate sources of uncertainty.

See [convergence estimates](../outputs/tables/monte_carlo_convergence_summary.csv)
and the [executable report](../report/portfolio_risk_analytics.qmd) for diagnostics.

## Interpretation

The experiment finds lower observed losses for copula-GARCH than for a portfolio
GARCH-t benchmark, alongside a clear sensitivity of exception frequencies to the
dependence assumption. Simple Gaussian and Student-t copulas remain competitive.
The evidence is strongest as a comparative account of these modelling choices on
one portfolio and period; the score intervals leave the generality of the gains open.

Weekly data retain a long calendar history but relatively few extreme events.
Bootstrap inference assumes sufficiently stable score differences and does not
account for selecting the experiment itself after observing its evaluation period.
FX, trading costs, changing portfolio weights and additional assets would change
the economic question being studied.

## Data and run record

All figures and estimates on this page refer to run
`full_20260920T230325.621650_26556`, with model source
`3ccf0d4abd4375152f614cd2a94a2a7b68c795b8`.

The [run provenance](../outputs/tables/run_provenance.csv) records source, data,
configuration and dependency identities. The [output manifest](../outputs/tables/output_manifest.csv)
records file hashes and sizes. [Reproduction](REPRODUCIBILITY.md) describes the
software environment, data access and local input archive.
