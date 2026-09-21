# Changelog

This file records software and experiment-design changes. The research narrative
and numerical findings are maintained in [README.md](README.md) and
[Results](docs/RESULTS.md).

## 0.2.0 — 2026-09-21

- A shared Friday valuation calendar with quote-date and staleness auditing.
- Exact empirical Expected Shortfall and signed loss-scale risk estimates.
- Common-date forecast scoring, missing-observation accounting and recoverable checkpoints.
- A portfolio GARCH-t benchmark and fixed-marginal copula experiments.
- Paired block-bootstrap score comparisons with marginal intervals and Holm adjustment.
- Input snapshots, run provenance, output integrity checks and an executable research report.
- A 520-date evaluation of 11 models at two confidence levels.

Earlier weekly results used intersections of exact quote dates, which could omit
holiday weeks. The current findings use a common valuation calendar and a different
evaluation design; historical ranks should not be treated as directly comparable.

## 0.1.1

- Consistent return-space handling and configured short-selling policy.
- Numerical checks, explicit model-failure accounting and copula fallback handling.
- Configuration-aware caching, resumable forecasts and random-state isolation.

## 0.1.0 — 2026-07-18

- Initial R package structure, data pipeline and model implementations.
- Rolling portfolio-risk forecasts, benchmark evaluation and Quarto reporting.
