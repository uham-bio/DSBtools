txt_en <- ev_txt("en")

test_that("apply_type() converts to nominal (unordered factor)", {
  out <- apply_type(c(1L, 2L, 1L), "nominal", "x", txt_en)
  expect_s3_class(out, "factor")
  expect_false(is.ordered(out))
  expect_identical(levels(out), c("1", "2"))

  # an ordered factor becomes unordered but keeps its level order
  ord <- factor(c("low", "high"), levels = c("low", "high"), ordered = TRUE)
  out2 <- apply_type(ord, "nominal", "x", txt_en)
  expect_false(is.ordered(out2))
  expect_identical(levels(out2), c("low", "high"))
})

test_that("apply_type() converts to ordinal (ordered factor)", {
  # unordered factor -> ordered, levels preserved
  f <- factor(c("b", "a", "c"), levels = c("a", "b", "c"))
  out <- apply_type(f, "ordinal", "x", txt_en)
  expect_true(is.ordered(out))
  expect_identical(levels(out), c("a", "b", "c"))

  # numeric -> ordered factor with numerically sorted levels, no message
  expect_no_message(
    out2 <- apply_type(c(3, 1, 2), "ordinal", "x", txt_en)
  )
  expect_true(is.ordered(out2))
  expect_identical(levels(out2), c("1", "2", "3"))
})

test_that("apply_type() messages about level order for non-numeric ordinal", {
  expect_message(
    apply_type(c("b", "a"), "ordinal", "grade", txt_en),
    "sorted alphabetically"
  )
})

test_that("apply_type() converts to discrete / continuous", {
  out <- apply_type(c(1, 2, 3), "discrete", "x", txt_en)
  expect_type(out, "double")
  expect_equal(out, c(1, 2, 3))

  # factor of numbers -> numeric
  out2 <- apply_type(factor(c("10", "20")), "continuous", "x", txt_en)
  expect_equal(out2, c(10, 20))

  # integer treated as continuous
  out3 <- apply_type(1:3, "continuous", "x", txt_en)
  expect_equal(out3, c(1, 2, 3))
})

test_that("apply_type() errors on impossible numeric conversions", {
  # non-numeric character cannot become discrete/continuous
  expect_error(
    apply_type(c("a", "b"), "continuous", "x", txt_en),
    "not numeric"
  )
  # non-whole values cannot become discrete
  expect_error(
    apply_type(c(1.5, 2.5), "discrete", "x", txt_en),
    "whole numbers"
  )
})

test_that("apply_type() error messages are localised", {
  txt_de <- ev_txt("de")
  expect_error(
    apply_type(c(1.5, 2.5), "discrete", "x", txt_de),
    "ganze Zahlen"
  )
})
