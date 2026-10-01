test_that("wt_suggest() recommends correlation for numeric vs numeric", {
  s <- wt_suggest(iris$Petal.Length, iris$Petal.Width)
  expect_identical(s$scenario, "assoc_num")
  expect_identical(s$test, "pearson")
  expect_identical(s$alt, "spearman")
})

test_that("wt_suggest() uses Spearman when a numeric variable is ordinal", {
  ord <- factor(c("low", "mid", "high"), levels = c("low", "mid", "high"),
    ordered = TRUE)
  s <- wt_suggest(ord, c(1.5, 2.5, 3.5))
  # ordinal vs numeric is a group comparison, not a correlation
  expect_identical(s$scenario, "groups")
})

test_that("wt_suggest() picks t-test vs ANOVA by group count", {
  s2 <- wt_suggest(factor(rep(c("a", "b"), 10)), (seq_len(20) + 0.5))
  expect_identical(s2$scenario, "groups")
  expect_equal(s2$n_groups, 2)
  expect_identical(s2$test, "ttest")
  expect_identical(s2$alt, "mannwhitney")

  s3 <- wt_suggest(iris$Species, iris$Sepal.Length)
  expect_equal(s3$n_groups, 3)
  expect_identical(s3$test, "anova")
  expect_identical(s3$alt, "kruskal")
})

test_that("wt_suggest() prefers rank-based tests for ordinal groups", {
  g2 <- factor(rep(c("low", "high"), 10), levels = c("low", "high"),
    ordered = TRUE)
  expect_identical(wt_suggest(g2, (seq_len(20) + 0.5))$test, "mannwhitney")

  g3 <- factor(rep(c("low", "mid", "high"), 6),
    levels = c("low", "mid", "high"), ordered = TRUE)
  expect_identical(wt_suggest(g3, (seq_len(18) + 0.5))$test, "kruskal")
})

test_that("wt_suggest() chooses chi-squared vs Fisher by expected counts", {
  # large, balanced table -> chi-squared
  big <- wt_suggest(rep(c("a", "b"), each = 50), rep(c("p", "q"), 50))
  expect_identical(big$scenario, "cat_cat")
  expect_identical(big$test, "chisq")

  # tiny table -> small expected counts -> Fisher
  small <- wt_suggest(c("a", "a", "b"), c("p", "q", "q"))
  expect_true(small$small_expected)
  expect_identical(small$test, "fisher")
})

test_that("wt_suggest() reports when there is only one group", {
  s <- wt_suggest(factor(rep("a", 5)), (seq_len(5) + 0.5))
  expect_identical(s$test, "none_one_group")
})

test_that("which_test() returns a printable suggestion object", {
  res <- which_test(iris$Species, iris$Sepal.Length)
  expect_s3_class(res, "ds_test_suggestion")
  expect_identical(res$test, "anova")
  expect_output(print(res), "One-way ANOVA")
  expect_output(print(res), "Kruskal-Wallis")
})

test_that("which_test() prints German output", {
  res <- which_test(iris$Species, iris$Sepal.Length, language = "de")
  expect_output(print(res), "Varianzanalyse")
})

test_that("which_test() validates lengths", {
  expect_error(which_test(1:5, 1:4), "same length")
})
