library(infercalcR)
# Every method has one provenance row: where its test comes from, which
# original computes it, and, for methods with their own base implementation,
# the literature it implements and the outcome of the GPL-provenance check.
provenance <- utils::read.csv(system.file("methods", "provenance.csv", package = "infercalcR", mustWork = TRUE),
  colClasses = "character")
stopifnot(identical(names(provenance), c("method_id", "engines", "validated_source", "original",
  "method_reference", "base_derivation", "gpl_check", "notes")))
catalogue <- method_catalogue()
stopifnot(setequal(provenance$method_id, catalogue$method_id), !anyDuplicated(provenance$method_id),
  identical(provenance$engines[match(catalogue$method_id, provenance$method_id)], catalogue$engines))
dual <- provenance[provenance$engines == "base+reference", , drop = FALSE]
stopifnot(nrow(dual) == 13L,
  all(nzchar(dual$validated_source)), all(nzchar(dual$method_reference)), all(nzchar(dual$base_derivation)),
  all(grepl("doi.org/10\\.", dual$method_reference)),
  identical(dual$original, catalogue$reference[match(dual$method_id, catalogue$method_id)]))
# pending: own base code outside the 13 dual-engine methods, audited with its reference engine later.
stopifnot(all(provenance$gpl_check %in% c("clear", "rewrite", "not_applicable", "pending")),
  all(dual$gpl_check %in% c("clear", "rewrite")))
# A single-engine method recorded as computed entirely by the original must not
# compute tests itself: none of its own functions calls a distribution function.
ns <- asNamespace("infercalcR")
own_tests <- function(id) {
  functions <- grep(paste0("^\\.obs_", id, "_"), ls(ns, all.names = TRUE), value = TRUE)
  any(vapply(functions, function(f) any(grepl("stats::(pt|pf|pnorm|pchisq|qt|qnorm|qchisq)\\(",
    deparse(get(f, envir = ns)))), NA))
}
wrapped <- provenance$method_id[provenance$engines == "single" & provenance$gpl_check == "not_applicable"]
computing <- wrapped[vapply(wrapped, own_tests, NA)]
if (length(computing)) stop("recorded as computed by the original but computes tests itself: ", paste(computing, collapse = ", "))
if (any(provenance$gpl_check == "rewrite"))
  stop("base implementations awaiting a rewrite from the literature: ",
    paste(provenance$method_id[provenance$gpl_check == "rewrite"], collapse = ", "))
cat("PASS provenance of", nrow(provenance), "methods\n")
