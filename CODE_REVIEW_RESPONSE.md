# Code Review Disposition

This document records the disposition of the independent code review completed on
2026-07-18. Findings were reproduced against the code rather than accepted solely from
the report.

## Accepted findings

| Finding | Disposition |
|---|---|
| H1: copula-GARCH risk and realised returns used different return spaces | Fixed. Simulated portfolio simple returns are converted with `log1p` before risk measurement whenever `portfolio.return_type` is `log`; rolling scoring and headline tables therefore use one configured model space. Both simple and model-space simulations remain available for inspection. |
| H2: native copula fallback could index past the score vector | Fixed. The fallback is protected, appended to the fit table, scored only when finite, and can fail as a structured candidate rather than aborting selection. |
| M1: price-cache identity omitted request fields | Fixed. The source, assets, date range, frequency, adjusted-price flag, and CSV identity are hashed and verified on cache reads. |
| M2: simulation bypassed the short-selling gate | Fixed. `portfolio.allow_short` is passed through the pipeline and enforced directly. |
| M4: native eGARCH Student-t centring and copula convergence | Fixed. The native Student-t eGARCH uses its unit-variance expected absolute innovation, and Student-t copula convergence reflects the optimiser result. |
| M5: resumed rolling runs could change the reselection schedule or retain failures | Fixed. Versioned checkpoints store fingerprints and successful state snapshots; resume keeps only a contiguous successful prefix, anchors reselection to the original origin index, and retries failed or incomplete origins. |
| M6: `_targets.R` duplicated and drifted from the runner | Fixed. Targets now delegates analysis to `run_analysis()` and fails on pipeline or render errors. |
| M7: Yahoo adjusted-price requests silently fell back to close | Fixed. Adjusted requests now fail explicitly when adjusted prices are unavailable; unadjusted close is used only when configured. Metadata records the selected field. |
| M8: configuration validation was incomplete | Fixed. Data, portfolio, risk, model, simulation, rolling, compute, and report fields now fail early with actionable messages. |

## Partially accepted finding

The native marginal engine is deliberately a two-step approximation, so its information
criterion is not a full joint ARMA-GARCH likelihood. The review was correct that IC values
should not be used across different residual-generating ARMA orders. Native selection is
now restricted to the least-complex eligible ARMA order, with an explicit warning; the
production `rugarch` path remains the preferred full-likelihood comparison.

## Additional accepted hardening

- Non-finite diagnostic p-values are retained as failed diagnostics instead of reaching
  `if (NA)`.
- Simulation helpers preserve the caller's RNG state and avoid reseeding between chunks.
  Results are deterministic for a fixed chunk size; changing it can remap the stream to
  bivariate draws.
- `risk_by_confidence()` forwards its quantile type, atomic RDS writes use unique temporary
  files, and fixture generation no longer changes global RNG state.
- Runner scripts resolve the repository root from their own location, so invocation does
  not depend on the current working directory.
- The pipeline names copula-GARCH explicitly for the rolling exceedance headline when that
  model is present, and plotting palettes scale beyond four assets.
- Dead orchestration settings were removed; live settings such as adjusted-price selection
  and report rendering are now validated and honoured.

## Finding not changed

The zero-exception Christoffersen independence statistic was not altered. At that boundary
there are no exception transitions from which to infer clustering, so the implemented
likelihood-ratio result is mathematically consistent. Coverage adequacy remains visible
through the Kupiec test, exception-rate ratio, and traffic-light classification; the
independence p-value is not used alone to declare a model adequate.

## Verification

Dedicated regression tests cover configured return-space consistency, the native copula
fallback, cache request identity, short-selling enforcement, resume equivalence, failed-row
retry, exact Student-t centring, non-finite diagnostics, simulation RNG isolation, ES
evaluation, model comparison, and Monte Carlo convergence. The full package test and
`R CMD check --no-manual` results are recorded in `STATUS.md`.
