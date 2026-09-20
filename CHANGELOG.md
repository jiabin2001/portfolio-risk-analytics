# Changelog

## 0.2.0 - 2026-09-21

- Preserve holiday weeks on shared Friday valuations; audit quote ages and enforce
  CSV date/frequency settings.
- Compute exact empirical ES tail mass and preserve signed statistical risk values.
- Compare losses on common dates, preserve missing transition gaps, and expose
  unidentifiable tests and FZ0-domain exclusions.
- Add portfolio GARCH-t, shared-marginal copula ablations, paired block-bootstrap
  intervals and multiplicity-adjusted comparative p-values.
- Treat fitted-sample PIT goodness-of-fit p-values as exploratory diagnostics.
- Strengthen cache/checkpoint identities and recovery; support empty baselines and
  correct realised simple-return rolling aggregation.
- Archive inputs and provenance, checksum outputs, and repair targets dependencies.
- Preserve generated CSV bytes across Git checkouts and audit committed artifacts in CI.
- Supersede irregular-horizon v0.1 results; replace heuristic composite ranking with
  separate predictive scores and runtime reporting.

## 0.1.1 - Previous review hardening

- Keep simulated portfolio risk and realised rolling returns in the configured return
  space, and enforce the configured short-selling policy during simulation.
- Repair native copula fallback selection, convergence reporting, and failed-fit
  accounting.
- Key data caches by the complete request identity and make adjusted-price fallbacks
  explicit instead of silently substituting close prices.
- Make rolling checkpoints schema- and input-aware, retry failures on resume, and retain
  selection state so resumed runs reproduce uninterrupted schedules.
- Add complete configuration validation, exact Student-t eGARCH centring, preserved RNG
  state, collision-safe atomic writes, and robust diagnostic handling.
- Remove duplicate targets orchestration in favour of the tested pipeline and add
  regression coverage for the reviewed failure paths.

## 0.1.0 - 2026-07-18

- Initial independent package architecture.
- Added configurable data, modelling, simulation, and backtesting modules.
- Added network-free tests and smoke-analysis orchestration.
