test_that("detect_type() maps storage classes to scales of measurement", {
  expect_identical(detect_type(c("a", "b", "c")),          "nominal")
  expect_identical(detect_type(c(TRUE, FALSE, NA)),        "nominal")
  expect_identical(detect_type(factor(c("a", "b"))),       "nominal")
  expect_identical(detect_type(factor(c("a", "b"),
                                      ordered = TRUE)),     "ordinal")
  expect_identical(detect_type(1:10),                       "discrete")
  expect_identical(detect_type(c(1.5, 2.5, 3.5)),           "continuous")
  # a double that happens to hold whole numbers is still continuous by storage
  expect_identical(detect_type(c(1, 2, 3)),                "continuous")
})
