# Observed-method extraction evidence

The source baseline is inferguidR commit e91938f77eaa991b69989b121bfd32c124c95db0.
`implemented.csv` records the exact current method input signatures and calculation backends.
`synthetic-baseline.rds` contains only the baseline's deterministic synthetic examples and finalized expected outputs; it is a trusted package regression fixture, not an interchange decoder.
`helper-oracles.rds` preserves independently derived Jonckheere-Terpstra enumeration and Gower distances from the baseline's named JSON fixtures. No patient observations are included.

`tools/methods/extract-source.R` is a development-time extraction audit trail. It extracts ordinary computational definitions, renames scoped helpers, and excludes all example execution and plots. The installed package does not source or evaluate advisor scripts. `capture-fixtures.R` executes the unchanged baseline only during development to capture regression evidence; it is never used by an installed analysis.

New top-level tests use only base R. Baseline parity verifies extraction, matched stats calls verify adapters, and independent enumerations/known values and scientific invariants provide additional scoped evidence. No universal scientific certification is claimed. Optional backend tests are required in the full lane (`INFER_SUITE_FULL=true`); ordinary package checks can report optional-backend skips separately.

The following numerical-only legacy assertions moved into tests/observed-legacy-helpers.R:

- test-advisor-jonckheere-code.R: integer trend score enumeration block, lines 4-15.
- test-advisor-structure-code.R: ARI invariance/exact cases, lines 35-36, and Gower edge-arithmetic block, lines 55-62.
- test-advisor-prediction-code.R: both-class guard and tied-score AUC case, lines 35-36.

They exercise the installed calc namespace's canonical helpers. Teaching-script wrappers no longer need to retain these numerical implementations. All other legacy tests remain in the guidance package until the coordinator maps their executed equivalents.
