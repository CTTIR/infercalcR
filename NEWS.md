# infercalcR 0.1.0

* Thirteen methods gain a reference engine. `method_spec(engine =
  "reference")` computes them with the validated original (the function
  DescrTab2 uses, Exact for Boschloo's test, or the established package);
  `engine = "base"` stays the default. The originals are optional, and a
  missing original is an error, never a silent fallback. Both engines are
  benchmarked on the originals' help-page examples, and
  `inst/methods/provenance.csv` records each method's source, original and
  literature reference.
* `anova_repeated_one_way_gg` treats a within-subject residual below the
  numerical resolution of the inputs as zero. An exactly additive occasion
  pattern previously left platform-dependent rounding residue and could
  return a meaningless F test instead of "not applicable".
* The README and every vignette now acknowledge and cite DescrTab2
  (Institute of Medical Biometry, Heidelberg University), which deeply
  inspired the suite, the Methodenberatung pages of the University of
  Zurich and G*Power; the README also cites R and the packages used.
* First CRAN submission.
* Add three vignettes: getting started, diagnostic accuracy and classifier
  metrics, and exact tests for two-by-two tables. A pkgdown documentation
  site is published at <https://cttir.github.io/infercalcR/>.
* The maintainer address is now raban.heller@uni-ulm.de.
* DESCRIPTION gains a fuller Description with a reference for Boschloo's
  test, and declares `Language: en-US`; `inst/WORDLIST` records the
  technical terms used in the documentation.
* `parallel_analysis()` and `predict_inference()` gain runnable examples.
* The GitHub Actions check matrix adds R-devel and R oldrel-1 on Linux.
* Add a CTTIR v5 hex logo in the statistical inference color shared with
  the other inference packages; `tools/branding/render.R` draws it in R.
* Introduce common finalized result/accessor contracts, explicit method
  specifications, descriptive summaries and declared-family p-value adjustment.
* Extract Boschloo, classifier, distribution and calculator engines from
  inferguidR while preserving historical numerical and identity contracts.
* Promote 68 executable analysis procedures and three callable helpers to
  production adapters with explicit optional backends and preserved scope.
* Add installed base-R contract, numerical, semantic, mapping and regression
  tests; retain existing numerical coverage during the suite transition.
