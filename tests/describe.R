library(infercalcR)
d <- data.frame(value = c(1, 3, NA, 9), group = c("a", "a", "b", NA),
  ordinal = ordered(c("low", "high", "low", NA), levels = c("low", "high")))
original <- d
x <- describe_data(d, c("value", "ordinal"), "group", labels = c(value = "Measurement"))
r <- result_data(x)
stopifnot(identical(original, d), identical(result_metadata(x)$kind, "descriptive"),
  all(is.na(r$p.value)), all(r$variable_type[r$variable_id == "ordinal"] == "categorical"))
a <- r[!is.na(r$group) & r$group == "a" & r$variable_id == "value" & r$estimand == "mean", ]
stopifnot(nrow(a) == 1L, a$estimate == 2, a$n == 2, a$n.missing == 0, a$label == "Measurement")
b <- r[!is.na(r$group) & r$group == "b" & r$variable_id == "value", ]
stopifnot(all(b$status == "undefined"), all(b$n == 0), all(b$n.missing == 1))
stopifnot(any(r$group_missing), all(r$proportion[r$variable_id == "ordinal" & !is.na(r$group) & r$group == "a"] == .5))
tools::assertError(describe_data(data.frame(x = c(1, Inf))), classes = "infercalcR_invalid_input")
tools::assertError(describe_data(d, c("value", "value")), classes = "infercalcR_invalid_input")
stopifnot(identical(result_data(run_inference(method_spec("descriptive_summary"), data = d)),
                    result_data(describe_data(d))))
cat("description checks completed\n")
