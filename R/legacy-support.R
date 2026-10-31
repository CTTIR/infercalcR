# Feature-local historical identity support; plain numeric objects need none.
.ic_legacy_need <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    .ic_abort(sprintf("Historical identity/decimal features require the optional package '%s'; install it explicitly before using this feature.", package), "missing_dependency")
  }
  invisible(TRUE)
}
.ig_schema_versions <- c(classification_spec = "inferguidR.classification_spec/0.1.0-draft",
                        observed_test_spec = "inferguidR.observed_test_spec/0.1.0-draft")
# Historical schema names remain stable; guid owns its document dispatch.
.ig_to_plain <- function(x) {
  if (is.object(x) && !inherits(x, "AsIs")) {
    if (inherits(x, "ig_status")) return(.ig_status_to_list(x))
    if (inherits(x, c("classification_spec", "observed_test_spec", "classification_result", "observed_test_result"))) return(.ig_unclass_frozen(x))
  }
  if (is.list(x) && !is.data.frame(x) && length(x)) {
    nms <- names(x); asis <- inherits(x, "AsIs")
    out <- lapply(unclass(x), .ig_to_plain); names(out) <- nms
    if (asis) out <- I(out)
    return(out)
  }
  x
}
# Lossless enough for in-memory cache identity; not serialized document identity.
.bl_cache_number <- function(x) sprintf("%.17g", as.double(x))
