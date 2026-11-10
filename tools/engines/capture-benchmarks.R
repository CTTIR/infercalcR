#!/usr/bin/env Rscript
# Capture benchmark values from the original packages (development only).
#
# Usage, from the package root with the development version installed:
#   Rscript tools/engines/capture-benchmarks.R <method_id> ... [--replace <method_id>]
#
# Each case runs the reference engine, which calls the original package, and
# stores the compared native fields with the package version, the call and the
# original's own output. A stored record is never changed silently: if a new
# capture differs, the script stops unless --replace names that method. Stored
# values are never regenerated to make a platform pass.
library(infercalcR)
source(file.path("tools", "engines", "benchmark-cases.R"))
args <- commandArgs(TRUE)
replace <- args[which(args == "--replace") + 1L]
methods <- unique(c(setdiff(args, c("--replace", replace)), replace))
methods <- methods[!is.na(methods)]
if (!length(methods)) stop("name at least one method_id")
unknown <- setdiff(methods, names(benchmark_cases))
if (length(unknown)) stop("no benchmark cases for: ", paste(unknown, collapse = ", "))

store <- file.path("inst", "methods", "engine-benchmarks.rds")
records <- if (file.exists(store)) readRDS(store) else list()
key <- function(r) paste(r$method_id, r$case_id)
same_values <- function(a, b) isTRUE(all.equal(a, b, tolerance = 0))

for (method_id in methods) {
  for (case in benchmark_cases[[method_id]]()) {
    spec <- do.call(method_spec, c(list(method_id), case$spec, list(engine = "reference")))
    result <- do.call(run_inference, c(list(spec), case$args))
    engine <- result_metadata(result)$engine
    native <- result$payload$native
    record <- list(method_id = method_id, case_id = case$case_id, source = case$source, spec = case$spec,
      args = case$args, package = engine$package, version = engine$version, call = engine$call,
      fields = case$fields, values = native[case$fields], raw = native$reference_output,
      captured_on = format(Sys.Date()), r_version = R.version.string)
    old <- Filter(function(r) identical(key(r), key(record)), records)
    if (length(old) && !same_values(old[[1L]]$values, record$values) && !method_id %in% replace)
      stop("captured values differ from the stored record for ", key(record), "; rerun with --replace ", method_id)
    records <- c(Filter(function(r) !identical(key(r), key(record)), records), list(record))
    cat("captured", key(record), "with", engine$package, engine$version, "\n")
  }
}
records <- records[order(vapply(records, key, ""))]
saveRDS(records, store, version = 2)
cat("stored", length(records), "records in", store, "\n")
