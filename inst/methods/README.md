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

## Engine benchmarks

Methods with a reference engine (`method_catalogue()$engines == "base+reference"`) are benchmarked against the validated original package. `engine-benchmarks.rds` holds one record per method and case: the input, its source (the original function's help-page example or this directory's synthetic baseline), the original package, version and call, the compared native fields and their values, and the original's own output. `tools/engines/capture-benchmarks.R` writes the records during development by running the reference engine; it refuses to change a stored record unless explicitly told to replace that method's records. `tests/engine-benchmarks.R` compares the base engine with every record at a relative difference of at most 1e-8. When the original is installed it also compares the original's current output with the stored values; the full lane (`INFER_SUITE_FULL=true`) requires the originals.

`engine-deviations.csv` lists every comparison that cannot meet that rule, with its kind (`monte_carlo`, `convention` or `original_defect`), the rule used instead, an explanation and the owner's approval. A deviation without an approval fails the benchmark test.

## Cross-runtime verification

The original R 4.6.1 synthetic fixture is never regenerated to make another platform pass. Common-row mapping compares against the current finalized native payload at tolerance 1e-12; frozen numerical regression is a separate test with method/field diagnostics.

`tests/support/observed-portability.R` permits only these explicit equivalences:

- Whole factor column sign/permutation changes transform factor correlations together. Factor covariance identities and variable names remain strict; PCA retains the original strict frozen comparison; elementwise absolute-value comparisons are not used.
- Ward trees retain identical clades, matched heights, cophenetic distances and the requested partition. Independent equal-height merges may be ordered differently, including partial cuts inside a tied block; all other cuts must agree.
- EFA optimized loadings/correlations/uniquenesses and reconstructed/residual correlations, and multinomial optimized coefficients/SE/derived coefficient rows/fitted probabilities, use a scoped absolute 1e-8 frozen bound. Observed R 4.1.3/4.6.2 backend differences were at most 1.4e-10 for EFA and 6.3e-9 for multinomial fields. EFA likelihood/covariance identities, multinomial softmax/information/Wald identities and the likelihood ratio are independently checked at 1e-12 (1e-10 for the LR), with score residual bounded by 1e-5. Other frozen fields retain the strict 1e-12 comparison.
- The logistic random-intercept fixture permits absolute changes of 1e-6 in fixed effects/random variance and 2e-5 in covariance only after strict current-runtime `glmer` parity. Independent Gaussian integration verifies marginal log likelihood within 1e-10 of the frozen optimum, analytic score below 1e-5, positive information, and native covariance within 1e-5 of the nuisance-adjusted inverse information. SE, statistic, p-value, odds ratio and interval algebra stay strict at 1e-12. Observed covariance drift was 1.1e-5, explained by the backend finite-difference Hessian: the two native covariances were within 6.9e-6 of independent information and likelihoods differed by 5.4e-13. No general mixed-model tolerance changes are made.
- The ordinal fixture keeps every field except its derived coefficient Wald p-value at the original 1e-12 frozen comparison. The stored p-value must equal its current-runtime formula exactly. Its frozen difference must be bounded by the normal-tail derivative over the observed z interval times the accepted z difference, plus an explicit relative allowance of 16 machine epsilons. The baseline z is recomputed from its estimate/SE, and its stored p must match the normal-tail formula within that same roundoff allowance. This is a scoped numerical allowance, not a universal certified library error bound. Captured macOS/Windows p differences were at most 1.05e-14; the corresponding z differences were at most 5.07e-13. Negative tests reject altered p-values and excessive estimate/SE drift even when their Wald formulas remain internally consistent.
- Signed-rank recorded confidence levels are checked against an explicit matched `stats::wilcox.test` call on the executing R. Some older R versions report nominal or rounded attributes; endpoints, estimates, p-values and captured diagnostics are preserved. Synthetic finalized level-1 validation remains mandatory on every R version, and small-sample endpoint support is checked independently.

`tests/observed-portability.R` deliberately corrupts amplitudes, uncouples signs, changes tree heights, and perturbs p-values to verify that the equivalence rules still reject genuine numerical regressions. `tools/methods/diagnose-portability.R` records all field deltas and runtime provenance; it is diagnostic only and does not decide acceptance or replace the full mandatory-capability lane.

On R 4.1.3, `summary.glm()` expands shared deferred string names and multinomial/ordinal factor operations expand deferred factor levels. Values were unchanged, but serialized inputs changed. Model conversion and the affected multinomial/ordinal adapters now copy these inputs before the operation; the original strict byte-preservation assertions remain active.
