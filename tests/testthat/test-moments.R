test_that("ds_skewness() is 0 for symmetric data", {
  expect_equal(ds_skewness(c(1, 2, 3, 4, 5)), 0)
  expect_equal(ds_skewness(c(-2, -1, 0, 1, 2)), 0)
})

test_that("ds_skewness()/ds_kurtosis() return NA for degenerate input", {
  expect_true(is.na(ds_skewness(rep(3, 10))))      # zero variance
  expect_true(is.na(ds_kurtosis(rep(3, 10))))
  expect_true(is.na(ds_skewness(c(1, 2))))         # fewer than 3 values
  expect_true(is.na(ds_kurtosis(c(1, 2, 3))))      # fewer than 4 values
})

test_that("ds_skewness()/ds_kurtosis() ignore missing values", {
  expect_equal(ds_skewness(c(1, 2, 3, 4, 5, NA)), ds_skewness(1:5))
  expect_equal(ds_kurtosis(c(1, 2, 3, 4, 5, NA)), ds_kurtosis(1:5))
})

test_that("ds_skewness()/ds_kurtosis() match the moments package", {
  skip_if_not_installed("moments")
  for (x in list(iris$Sepal.Length, airquality$Wind,
    c(1, 1, 2, 3, 5, 8, 13, 21))) {
    expect_equal(ds_skewness(x), moments::skewness(x))
    expect_equal(ds_kurtosis(x), moments::kurtosis(x) - 3)  # excess kurtosis
  }
})
