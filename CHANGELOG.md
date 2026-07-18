# Changelog

## Unreleased

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
