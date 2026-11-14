test_that("large rank-sum groups retain descriptive superiority estimates", {
  cases <- list(
    list(x = rep(0, 50000), y = rep(0, 50000), expected = 0.5),
    list(x = rep(1, 50000), y = rep(0, 50000), expected = 1),
    list(x = rep(0, 50000), y = rep(1, 50000), expected = 0),
    list(x = rep(c(0, 1), each = 25000), y = rep(0, 50000), expected = 0.75)
  )
  for (case in cases) {
    expect_no_warning(out <- run_inference(method_spec("rank_sum_mann_whitney"),
      x = case$x, y = case$y, exact = FALSE))
    native <- out$payload$native
    expect_equal(native$probability_superiority, case$expected)
    expect_equal(result_data(out)$estimate[1L], case$expected)
    if (native$status == "not_applicable") {
      expect_null(native$p_value)
    } else {
      expect_equal(native$rank_biserial, 2 * case$expected - 1)
      reference <- stats::wilcox.test(case$x, case$y, exact = FALSE, correct = TRUE)
      expect_identical(native$p_value, reference$p.value)
    }
  }
})

test_that("small rank-sum superiority agrees with direct pair counting", {
  for (x in list(c(1, 4, 6), c(0, 0, 1), c(-2, 3))) {
    for (y in list(c(2, 3, 5, 7), c(0, 1, 1), c(-1, 4))) {
      pairs <- outer(x, y, "-")
      expected <- mean((pairs > 0) + 0.5 * (pairs == 0))
      out <- run_inference(method_spec("rank_sum_mann_whitney"),
                           x = x, y = y, exact = FALSE)
      expect_equal(out$payload$native$probability_superiority, expected)
      expect_equal(result_data(out)$estimate[1L], expected)
    }
  }
})
