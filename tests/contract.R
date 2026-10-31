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
invalid(new_inference_result(data.frame(row_id = c("a", "a")), list(method_id = "test")))
invalid(new_inference_result(data.frame(p.value = 2), list(method_id = "test")))
invalid(new_inference_result(data.frame(n = -1), list(method_id = "test")))
invalid(new_inference_result(data.frame(conf.low = 3, conf.high = 2), list(method_id = "test")))
invalid(method_spec("not_registered"))
invalid(method_spec("descriptive_summary", options = list(unknown = TRUE)))
invalid(method_spec("descriptive_summary", conf.level = 1))
z <- new_inference_result(data.frame(estimate = NA_real_, p.value = NA_real_,
  status = "undefined", reason = "no observations"), list(method_id = "fixture"))
stopifnot(is.na(result_data(z)$p.reported), result_data(z)$status == "undefined")
tmp <- tempfile(); saveRDS(x, tmp); stopifnot(identical(readRDS(tmp), x)); unlink(tmp)
cat("contract checks completed\n")
