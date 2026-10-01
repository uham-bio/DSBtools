txt_en <- ev_txt("en")

# -- mean_ci ------------------------------------------------------------------

test_that("mean_ci() always uses the t-distribution", {
  v  <- c(1, 2, 3, 4, 5)
  ci <- mean_ci(v)
  expect_equal(ci$n, 5)
  expect_equal(ci$mean, mean(v))
  expect_equal(ci$se, sd(v) / sqrt(length(v)))
  crit <- qt(0.975, df = length(v) - 1)
  expect_equal(ci$lo, mean(v) - crit * ci$se)
  expect_equal(ci$hi, mean(v) + crit * ci$se)
  expect_null(ci$method)                       # method label removed
})

test_that("mean_ci() uses t even for n >= 30 (not the normal approximation)", {
  v    <- as.numeric(1:40)
  ci   <- mean_ci(v)
  crit <- qt(0.975, df = length(v) - 1)         # t, not qnorm()
  expect_equal(ci$lo, mean(v) - crit * ci$se)
  expect_equal(ci$hi, mean(v) + crit * ci$se)
})

# -- display formatters -------------------------------------------------------

test_that("fmt_num_table() orders rows by role and drops the CI method label", {
  row <- data.frame(n = 5, mean = 3, median = 3, sd = 1.581, var = 2.5,
    iqr = 2, min = 1, max = 5, skewness = 0, kurtosis = -1.3,
    se = 0.707, ci_lower = 1.04, ci_upper = 4.96)
  tbl <- fmt_num_table(row, "V", txt_en, 3)
  expect_named(tbl, c("Statistic", "V"))
  expect_equal(tbl$Statistic,
    c("N", "Mean", "Median", "SD", "Variance", "IQR", "Min / Max",
      "Skewness", "Kurtosis", "SE (mean)", "95% CI"))
  expect_equal(tbl$V[tbl$Statistic == "N"], "5")
  expect_equal(tbl$V[tbl$Statistic == "Skewness"], "0")
  expect_false(any(grepl("\\[", tbl$Statistic)))   # no "[z]" / "[t, df=..]"
})

test_that("fmt_num_table() honours sig_digits and localises labels", {
  row <- data.frame(n = 3, mean = 2, median = 2, sd = 1.52753, var = 2.333,
    iqr = 1.5, min = 1, max = 4, skewness = 0.1, kurtosis = -0.2,
    se = 0.8819, ci_lower = -1.8, ci_upper = 5.8)
  expect_equal(fmt_num_table(row, "V", txt_en, 3)$V[4],
    as.character(signif(1.52753, 3)))
  expect_equal(fmt_num_table(row, "V", txt_en, 5)$V[4],
    as.character(signif(1.52753, 5)))
  expect_true("Mittelwert" %in% fmt_num_table(row, "V", ev_txt("de"), 3)$Kennzahl)
})

test_that("fmt_group_table() includes n_missing, skewness, kurtosis and orders columns by role", {
  g <- tibble::tibble(group = c("a", "b"), n = c(2L, 3L),
    n_missing = c(0L, 1L), mean = c(2, 4),
    median = c(2, 4), sd = c(1.41, 2), var = c(2, 4), iqr = c(1, 2),
    min = c(1, 2), max = c(3, 6), skewness = c(0, 0.5), kurtosis = c(-1, 2),
    se = c(1, 1.15), ci_lower = c(-1, 0.5), ci_upper = c(5, 7.5))
  out <- fmt_group_table(g, txt_en, 3)
  expect_equal(names(out),
    c("Group", "N", "missing", "Mean", "Median", "SD", "Variance", "IQR",
      "Min / Max", "Skewness", "Kurtosis", "SE (mean)", "95% CI"))
  expect_equal(out[["Variance"]], c("2", "4"))
  expect_equal(out[["Skewness"]], c("0", "0.5"))
})

test_that("fmt_freq_table() shows rel as a rounded percentage", {
  f   <- tibble::tibble(category = c("a", "b"), n = c(3L, 1L), rel = c(0.75, 0.25))
  out <- fmt_freq_table(f, txt_en)
  expect_named(out, c("Category", "N (abs.)", "N (rel.)"))
  expect_equal(out[[3]], c("75 %", "25 %"))
})

# -- number formatters --------------------------------------------------------

test_that("fmt_num() keeps whole numbers integer and rounds floats", {
  expect_equal(fmt_num(c(3, 3.14159, NA), 3), c("3", "3.14", NA))
  expect_equal(fmt_num(10, 3), "10")
})

test_that("fmt_p() formats p-values", {
  expect_identical(fmt_p(0.0001), "< 0.001")
  expect_identical(fmt_p(0.0300), "= 0.030")
  expect_identical(fmt_p(NA_real_), "= NA")
})
