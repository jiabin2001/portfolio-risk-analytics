# Portfolio Risk Analytics

**Volatility, dependence and portfolio tail risk**

An empirical study of weekly Value-at-Risk (VaR) and Expected Shortfall (ES) for a
UK–US equity portfolio. The project combines conditional volatility models,
copula-based joint scenarios and rolling out-of-sample evaluation in R.

The central question is whether modelling dependence between asset returns improves
portfolio tail-risk forecasts beyond a univariate volatility model.

[Findings](docs/RESULTS.md) · [Methodology](vignettes/methodology.qmd) ·
[Reproduction](docs/REPRODUCIBILITY.md) · [Research report](report/portfolio_risk_analytics.qmd)

## Study design

| Component | Specification |
| --- | --- |
| Portfolio | Equal-weight FTSE 100 and S&P 500 local-index returns |
| Data | 1,356 weekly valuations, January 2000–December 2025 |
| Forecast horizon | One week; VaR and ES at 95% and 99% confidence |
| Evaluation | 520 forecast dates, 15 January 2016–26 December 2025 |
| Estimation | A moving 520-week window; specifications reselected every 65 weeks |
| Comparison | 11 models, scored on the same out-of-sample dates |

The model estimates each index's conditional return distribution with ARMA–GARCH,
transforms residuals to uniform margins, and fits a bivariate copula. Joint simulations
are mapped back to asset returns and aggregated into portfolio losses.

Three comparisons give the experiment its structure:

- **Volatility:** historical, Gaussian, Student-t, EWMA and filtered-historical
  forecasts provide benchmarks alongside a portfolio GARCH-t model.
- **Dependence:** independence, Gaussian, Student-t and BB1 copulas share the same
  fitted asset marginals, isolating the effect of the dependence assumption.
- **Predictive value:** quantile loss and a joint VaR/ES score measure forecast
  accuracy; coverage tests and paired block-bootstrap intervals describe calibration
  and uncertainty.

## Findings

Copula-GARCH produces lower average forecast losses than portfolio GARCH-t in this
sample. The observed VaR improvements are:

| VaR confidence | GARCH-t quantile loss | Copula-GARCH quantile loss | Reduction |
| --- | ---: | ---: | ---: |
| 95% | 0.002420 | 0.002369 | **2.12%** |
| 99% | 0.000815 | 0.000760 | **6.80%** |

Joint VaR/ES scores also favour copula-GARCH in the sample. The corresponding 95%
intervals for all four score differences span zero, leaving the size and persistence
of the advantage uncertain. The [results note](docs/RESULTS.md) reports effect sizes,
intervals and the complete comparison design.

Dependence assumptions affect the observed calibration: at the 99% level, the
independence ablation records 15 exceptions in 520 weeks, compared with four for
copula-GARCH. Gaussian and Student-t copulas are competitive with the selected-family
model, making the choice of dependence structure a substantive part of the study.

![Weekly portfolio returns and rolling 99% copula-GARCH VaR thresholds](outputs/figures/rolling_var_exceedances.png)

*One-week-ahead forecasts across the evaluation period. Each forecast uses only
information available before its forecast date.*

## Reproduce the study

Use **R 4.5.1** and **Quarto**. From the repository root:

```sh
Rscript -e "install.packages('renv'); renv::restore()"
Rscript scripts/run_full_analysis.R
```

The full profile downloads or reuses cached market data, fits the models, runs the
rolling experiment and renders `report/portfolio_risk_analytics.html`. Its settings
are in [config/full.yml](config/full.yml).

The committed CSV tables and figures can also be inspected without fitting models.
Their integrity check requires only base R:

```sh
Rscript --vanilla scripts/audit_outputs.R
```

For installation details, an offline smoke run, tests and the `targets` workflow,
see [Reproduction](docs/REPRODUCIBILITY.md).

## Research materials

| Material | Contents |
| --- | --- |
| [Results](docs/RESULTS.md) | Forecast losses, calibration, dependence comparisons and uncertainty |
| [Methodology](vignettes/methodology.qmd) | Return construction, model specification and scoring conventions |
| [Report](report/portfolio_risk_analytics.qmd) | Executable analysis with tables, diagnostics and figures |
| [Output tables](outputs/tables) | Per-date forecasts, paired scores, valuation audit and run provenance |
| [R source](R) | Data, marginal models, copulas, simulation and forecast evaluation |
| [Tests](tests/testthat) | Numerical checks and pipeline regression coverage |

## Scope

The study concerns a bivariate, weekly, equal-weight portfolio of local-currency
index returns. FX conversion, transaction costs and a holdings ledger are outside
the experiment. Inference is conditional on the observed historical period and the
chosen model set; 520 weekly forecasts provide limited information about rare tail
events. Data conventions and inference assumptions are set out in the methodology.

## Methodological background

- Patton (2006), [Modelling Asymmetric Exchange Rate Dependence](https://public.econ.duke.edu/~ap172/Patton_IER_2006.pdf).
- Patton, Ziegel and Chen (2019), [Dynamic Semiparametric Models for Expected Shortfall (and Value-at-Risk)](https://public.econ.duke.edu/~ap172/Patton_Ziegel_Chen_JoE_2019.pdf).

MIT licensed. See [LICENSE.md](LICENSE.md) and [Contributing](CONTRIBUTING.md).
