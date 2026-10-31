library(infercalcR)
invalid <- function(expr) tools::assertError(expr, classes = "infercalcR_invalid_input")
d <- data.frame(row_id = c("a", "b"), estimate = c(2, 3), p.value = c(.01, .04),
                conf.low = c(-Inf, 1), conf.high = c(3, Inf), conf.level = .95)
x <- new_inference_result(d, list(method_id = "fixture", validation = "synthetic contract fixture"))
stopifnot(inherits(x, "inference_result"), is.data.frame(result_data(x)),
  identical(result_data(x)$p.value, d$p.value), identical(result_metadata(x)$kind, "inference"),
  is.infinite(result_data(x)$conf.low[1]), identical(as_inference(x), x))
copy <- x
y <- adjust_inference(x, "holm", family_id = "prespecified", rows = c("a", "b"))
stopifnot(identical(x, copy), isTRUE(all.equal(result_data(y)$p.adjusted, c(.02, .04))),
  identical(result_data(y)$family_size, c(2L, 2L)), identical(result_data(y)$conf.low, d$conf.low))
invalid(adjust_inference(y, "holm", family_id = "again"))
family <- new_inference_result(data.frame(row_id=c("a","b","c"),p.value=c(.01,.02,.03)),
  list(method_id="three-prespecified-tests"))
first <- adjust_inference(family, family_id="primary", rows=c("a","b"))
before <- serialize(first,NULL)
invalid(adjust_inference(first, family_id="primary", rows="c"))
stopifnot(identical(serialize(first,NULL),before))
separate <- adjust_inference(first, family_id="secondary", rows="c")
stopifnot(identical(separate$data$family_id,c("primary","primary","secondary")),
  isTRUE(all.equal(separate$data$p.adjusted,c(.02,.02,.03))))
# Filtering does not erase the complete-family identity retained in metadata.
filtered <- first; filtered$data <- filtered$data[3L,,drop=FALSE]
invalid(adjust_inference(filtered, family_id="primary", rows="c"))
invalid(new_inference_result(data.frame(row_id = c("a", "a")), list(method_id = "test")))
invalid(new_inference_result(data.frame(p.value = 2), list(method_id = "test")))
invalid(new_inference_result(data.frame(n = -1), list(method_id = "test")))
invalid(new_inference_result(data.frame(estimate = NaN), list(method_id = "test")))
invalid(new_inference_result(data.frame(conf.low = 3, conf.high = 2), list(method_id = "test")))
malformed <- x
malformed$metadata$kind <- NULL
invalid(validate_inference(malformed))
invalid(method_spec("not_registered"))
invalid(method_spec("descriptive_summary", options = list(unknown = TRUE)))
invalid(method_spec("descriptive_summary", conf.level = 1))
z <- new_inference_result(data.frame(estimate = NA_real_, p.value = NA_real_,
  status = "undefined", reason = "no observations"), list(method_id = "fixture"))
stopifnot(is.na(result_data(z)$p.reported), result_data(z)$status == "undefined")
empty <- new_inference_result(data.frame(), list(method_id = "empty"))
stopifnot(nrow(result_data(empty)) == 0L, identical(result_data(empty)$row_id, character()))
tmp <- tempfile(); saveRDS(x, tmp); stopifnot(identical(readRDS(tmp), x)); unlink(tmp)
cat("contract checks completed\n")
