# Benchmark cases for the reference engines (development only).
#
# Every input comes from the original function's help-page example or from the
# method's entry in inst/methods/synthetic-baseline.rds. A case records where its
# input comes from. No input is made up for a benchmark.
#
# A case is list(case_id, source, spec = list(alternative, conf.level, options),
# args = <run_inference() arguments>, fields = <native fields to compare>).

baseline_case <- function(method_id, fields, case_id = "synthetic-baseline") {
  fixtures <- readRDS(file.path("inst", "methods", "synthetic-baseline.rds"))
  a <- fixtures[[method_id]]$arguments
  list(case_id = case_id, source = paste0("inst/methods/synthetic-baseline.rds: ", method_id),
    spec = list(alternative = if (is.null(a$alternative)) "two.sided" else a$alternative,
      conf.level = if (is.null(a$conf.level)) 0.95 else a$conf.level, options = list()),
    args = a[setdiff(names(a), c("alternative", "conf.level"))], fields = fields)
}

# DescTools CochranQTest help example: drug response data from the SAS/STAT
# FREQ documentation (F = favourable coded 1, U = unfavourable coded 0), built
# with expand.grid in the same cell order as the example's xtabs call.
cochran_q_help_data <- function() {
  cells <- expand.grid(A = c(1, 0), B = c(1, 0), C = c(1, 0))
  x <- cells[rep(seq_len(8L), c(6, 2, 2, 6, 16, 4, 4, 6)), ]
  data.frame(id = sprintf("subject_%02d", seq_len(nrow(x))), x, row.names = NULL)
}

benchmark_cases <- list(
  cochran_q = function() {
    q_fields <- c("status", "statistic", "df", "p_value")
    help <- cochran_q_help_data()
    concordant <- help[rowSums(help[c("A", "B", "C")]) %in% c(0, 3), ]
    list(
      list(case_id = "help-example", source = "DescTools 0.99.60 ?CochranQTest example (SAS/STAT FREQ documentation drug response data)",
        spec = list(alternative = "two.sided", conf.level = 0.95, options = list()),
        args = list(data = help), fields = q_fields),
      list(case_id = "help-example-concordant-blocks", source = "DescTools 0.99.60 ?CochranQTest example, concordant blocks only (degenerate case)",
        spec = list(alternative = "two.sided", conf.level = 0.95, options = list()),
        args = list(data = concordant), fields = "status"),
      baseline_case("cochran_q", q_fields))
  }
)
