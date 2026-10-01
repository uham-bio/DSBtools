cat_summary_cols <- c("variable", "type", "n", "n_missing", "n_categories",
  "mode", "mode_rel")

test_that("describe_df() summarises every column into two tibbles", {
  d <- describe_df(iris)
  expect_s3_class(d, "ds_description")
  expect_named(d, c("numeric", "categorical"))
  expect_equal(nrow(d$numeric), 4L)
  expect_setequal(d$numeric$variable,
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))
  # categorical default: one summary row per categorical variable
  expect_named(d$categorical, cat_summary_cols)
  expect_equal(nrow(d$categorical), 1L)
  expect_equal(d$categorical$variable, "Species")
  expect_equal(d$categorical$type, "nominal")
  expect_equal(d$categorical$n_categories, 3L)
})

test_that("describe_df() reuses the same numbers as describe_var()", {
  d1  <- describe_df(iris)$numeric
  d2  <- describe_var(iris$Sepal.Length)$numeric
  row <- d1[d1$variable == "Sepal.Length", ]
  expect_equal(row$mean, d2$mean)
  expect_equal(row$ci_lower, d2$ci_lower)
})

test_that("describe_df(frequencies = TRUE) returns long frequencies and lumps", {
  df <- data.frame(g = factor(rep(letters[1:12], each = 2)),
    stringsAsFactors = FALSE)
  d  <- describe_df(df, frequencies = TRUE, max_categories = 5)
  expect_named(d$categorical, c("variable", "category", "n", "rel"))
  expect_equal(nrow(d$categorical), 5L)                 # 4 kept + (other)
  expect_true("(other)" %in% d$categorical$category)
  expect_equal(sum(d$categorical$n), 24L)               # counts still sum
  expect_equal(sum(d$categorical$rel), 1)               # proportions still sum
})

test_that("describe_df() groups by categorical columns and drops them", {
  d <- describe_df(warpbreaks, by = "wool")
  expect_equal(names(d$numeric)[1], "wool")
  expect_equal(names(d$categorical)[1], "wool")
  expect_equal(nrow(d$numeric), 2L)                     # breaks x 2 wool levels
  expect_setequal(d$numeric$variable, "breaks")
  expect_false("wool" %in% d$numeric$variable)
  expect_setequal(d$categorical$variable, "tension")   # tension summarised
})

test_that("describe_df(frequencies = TRUE) computes rel within each group", {
  d <- describe_df(warpbreaks, by = "wool", frequencies = TRUE)
  by_grp <- tapply(d$categorical$rel, d$categorical$wool, sum)
  expect_true(all(abs(by_grp - 1) < 1e-8))
})

test_that("describe_df() emits identifier and type hints", {
  df <- data.frame(
    exp_id = sprintf("id%03d", 1:20),                   # 20 distinct in 20 rows
    code   = rep(1:3, length.out = 20),                 # integer, few distinct
    stringsAsFactors = FALSE)
  msgs <- catch_messages(describe_df(df))
  expect_match(msgs, "'exp_id' looks like an identifier \\(20 distinct", all = FALSE)
  expect_match(msgs, "'code' is stored as integer", all = FALSE)
})

test_that("describe_df() validates its input", {
  expect_error(describe_df(1:10), "data frame")
  expect_error(describe_df(iris, by = 1), "character vector")
  expect_error(describe_df(iris, by = "nope"), "not found")
  expect_error(describe_df(iris, by = "Sepal.Length"), "must be categorical")
})

test_that("describe_df() returns empty-but-typed tibbles when a kind is absent", {
  only_num <- data.frame(a = c(1.5, 2.5), b = c(2.5, 4.5))
  d <- describe_df(only_num)
  expect_equal(nrow(d$categorical), 0L)
  expect_named(d$categorical, cat_summary_cols)         # summary columns
})
