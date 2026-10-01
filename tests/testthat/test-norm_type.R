test_that("norm_type() returns NULL for NULL input", {
  expect_null(norm_type(NULL, "x_type"))
})

test_that("norm_type() matches full and partial type names", {
  expect_identical(norm_type("nominal", "x_type"), "nominal")
  expect_identical(norm_type("ordinal", "x_type"), "ordinal")
  expect_identical(norm_type("discrete", "x_type"), "discrete")
  expect_identical(norm_type("continuous", "x_type"), "continuous")
  # partial matching
  expect_identical(norm_type("cont", "x_type"), "continuous")
  expect_identical(norm_type("nom", "x_type"), "nominal")
  # case-insensitive
  expect_identical(norm_type("Continuous", "x_type"), "continuous")
})

test_that("norm_type() rejects ambiguous or unknown strings", {
  expect_error(norm_type("xyz", "x_type"), "must be one of")
  # "o" is ambiguous between ordinal ... (only ordinal starts with o) -> matches
  # but "n" is unambiguous? nominal only. Use a clearly invalid one:
  expect_error(norm_type("cat", "y_type"), "y_type")
})

test_that("norm_type() validates the input shape", {
  expect_error(norm_type(1, "x_type"), "single character string")
  expect_error(norm_type(c("a", "b"), "x_type"), "single character string")
  expect_error(norm_type(NA_character_, "x_type"), "single character string")
})
