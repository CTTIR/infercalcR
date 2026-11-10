library(infercalcR)
# Benchmarks of the base engine against values captured from the validated
# original packages (inst/methods/engine-benchmarks.rds). Inputs come from the
# originals' help-page examples and the stored synthetic baseline. Agreement is
# a relative difference of at most 1e-8; other comparisons need an approved row
# in inst/methods/engine-deviations.csv. When an original is installed, its
# current output is also compared with the stored values.
records <- readRDS(system.file("methods", "engine-benchmarks.rds", package = "infercalcR", mustWork = TRUE))
deviations <- utils::read.csv(system.file("methods", "engine-deviations.csv", package = "infercalcR", mustWork = TRUE),
  colClasses = "character")
stopifnot(identical(names(deviations), c("method_id", "case_id", "field", "kind", "rule", "explanation",
  "approved_by", "approved_on")), length(records) > 0L)
full <- identical(Sys.getenv("INFER_SUITE_FULL"), "true")
registry <- infercalcR:::.ic_engine_info

agree <- function(a, b) {
  if (is.null(a) || is.null(b)) return(is.null(a) && is.null(b))
  if (is.character(a) || is.character(b)) return(identical(as.character(a), as.character(b)))
  a <- as.double(unlist(a)); b <- as.double(unlist(b))
  if (length(a) != length(b)) return(FALSE)
  all(mapply(function(a, b) if (is.na(b)) is.na(a) else if (b == 0) isTRUE(a == 0) else
    isTRUE(abs(a - b) / abs(b) <= 1e-8), a, b))
}
native_values <- function(record, engine) {
  spec <- do.call(method_spec, c(list(record$method_id), record$spec, list(engine = engine)))
  result <- do.call(run_inference, c(list(spec), record$args))
  stopifnot(identical(result_metadata(result)$engine$engine, engine))
  result$payload$native[record$fields]
}

for (record in records) {
  label <- paste(record$method_id, record$case_id)
  base <- native_values(record, "base")
  for (field in record$fields) {
    rule <- deviations[deviations$method_id == record$method_id & deviations$field == field &
      deviations$case_id %in% c(record$case_id, "*"), , drop = FALSE]
    if (nrow(rule)) {
      if (!nzchar(rule$approved_by[[1L]])) stop("deviation awaiting owner approval: ", label, " ", field)
      # Rules are R expressions from the package's own owner-approved CSV, not user input.
      ok <- eval(parse(text = rule$rule[[1L]]),
        c(list(a = base[[field]], b = record$values[[field]]), record$args, record$spec$options))
    } else ok <- agree(base[[field]], record$values[[field]])
    if (!isTRUE(ok)) stop("base engine differs from ", record$package, " ", record$version, ": ", label, " ", field)
  }
  entry <- registry[[record$method_id]]
  live <- !is.null(entry) && requireNamespace(entry$package, quietly = TRUE) &&
    utils::packageVersion(entry$package) >= entry$min_version
  if (live) {
    reference <- native_values(record, "reference")
    same <- vapply(record$fields, function(field) agree(reference[[field]], record$values[[field]]), NA)
    if (!all(same)) {
      note <- paste0("live reference differs from stored ", record$package, " ", record$version, ": ", label, " ",
        paste(record$fields[!same], collapse = ", "))
      if (full) stop(note) else cat("NOTE", note, "\n")
    }
  } else {
    if (full) stop("full lane requires the original for ", label)
    cat("SKIP live reference:", record$package, "not installed for", label, "\n")
  }
  cat("PASS", label, "\n")
}
