# infercalcR

Explicit observed statistical procedures, diagnostic calculations and finalized
results in R. `infercalcR` supplies the computational core of the inference suite;
`inferguidR` provides guidance and prospective design planning, and `infereportR`
provides tables, text and plot annotations.

This is development software. Use the checked revision recorded with an analysis.
A successful calculation is not universal scientific validation. The package is
not a CRAN release.

## Start

```r
spec <- infercalcR::method_spec("t_means_two_independent_welch")
r <- infercalcR::run_inference(spec,
  x = c(5.2, 6.1, 4.9, 7.3, 5.8, 6.5, 7.1, 4.7),
  y = c(3.8, 5.9, 4.2, 5.1, 6.7, 4.8, 3.9, 5.3, 6.0),
  result_id = "primary-comparison")
infercalcR::result_data(r)
infercalcR::result_metadata(r)
infercalcR::describe_data(data.frame(value = c(1, 2, NA),
  category = factor(c("a", "b", "a"))))
```

The mean difference is x minus y. Sidedness, confidence level and method options
are explicit; data-driven pretests never choose the procedure. Inspect status,
reason, estimand, orientation and population before using an estimate. Missing
values, unsupported inference, numerical failure and legitimate infinite limits
remain distinct.

`method_catalogue()` lists current procedure identities, available adapters and
optional backends. `?observed-methods` and
[the input-signature inventory](inst/methods/implemented.csv) describe each
method's accepted data/design arguments. The 68 executable analysis templates
and their three callable numerical helpers were extracted with their original
scope, assumptions and limits. The advisor-only one-sample variance method is
explicitly unsupported as an observed adapter.

## Results and reporting

`as_inference()` explicitly converts supported htest, lm/glm, Boschloo and
classifier results. It never chooses or fits a replacement analysis. Model
conversion finalizes its declared numerical summaries once, before reporting.
Unknown sample sizes are not inferred from degrees of freedom.

The common result is an ordinary S3 object containing finalized data, metadata
and method-specific payload. Raw and adjusted p-values remain separate.
`adjust_inference()` requires a declared complete family and rejects silent
readjustment. Reporting may select rows but cannot change that family. See
[CONTRACT.md](CONTRACT.md) for the shared schema and legacy boundaries.

```r
# Optional reporting consumer; the calculation object needs no raw observations.
infereportR::report_table(r)
infereportR::report_text(r)
```

## Dependencies and installation

R 4.1.0 or later is required. The new result, descriptive and ordinary statistical
core uses only R-distributed base packages. The package never installs anything
at runtime. It has no dependency on either suite consumer.

Legacy identity-bearing functions retain optional digest/jsonlite for canonical
JSON, SHA-256 fingerprints and correctly rounded decimal parsing. They fail
explicitly if those packages are absent. `boschloo_inference()` uses the same
numerical engine without requiring historical identities. Plain count metrics
also work without contributed packages.

Specialist procedures explicitly use cluster, nlme, MASS, nnet, lme4 or geepack.
These are feature-local optional dependencies, with separate executed validation
lanes. Installing another package never silently switches the model, covariance
method or interval. Existing testthat regression coverage is a development
integration lane; new tests are ordinary R scripts.

The development repository is private. With authenticated access, install a
reviewed commit with your usual GitHub R installer. From a local checkout:

```sh
R CMD build infercalcR
R CMD INSTALL infercalcR_0.0.0.9000.tar.gz
R CMD check --no-manual infercalcR_0.0.0.9000.tar.gz
```

Install this provider before either consumer. The source archive includes
NAMESPACE and Rd help; installation needs no documentation generator. Set
`INFER_SUITE_FULL=true` for the required supported-backend tests. Minimal checks
may explicitly skip optional integration lanes and cannot substitute for full
validation.

## Evidence and provenance

New tests compare matched stats calls, historical synthetic fixtures, independent
small-support enumeration and scientific invariants. Backend parity verifies the
adapter, rather than independently certifying the backend algorithm. Historical
JSON remains readable through inferguidR with unchanged identities; new results
can legitimately record new producing-package provenance. Trusted RDS regression
fixtures are not an untrusted interchange format.

Raban Heller. MIT license; see [LICENSE.md](LICENSE.md). Record package versions,
repository revisions, method settings and sessionInfo() with reproducible work.
