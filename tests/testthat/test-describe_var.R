num_cols <- c("variable", "n", "n_missing", "mean", "median", "sd", "var",
  "iqr", "min", "max", "skewness", "kurtosis", "se", "ci_lower", "ci_upper")
cat_cols <- c("variable", "category", "n", "rel")

test_that("describe_var() always returns two tibbles", {
  d <- describe_var(iris$Sepal.Length)
  expect_s3_class(d, "ds_description")
  expect_named(d, c("numeric", "categorical"))
  expect_s3_class(d$numeric, "tbl_df")
  expect_s3_class(d$categorical, "tbl_df")
})

test_that("describe_var() summarises one numeric variable", {
  d <- describe_var(c(1, 2, 3, 4, 5))
  expect_equal(names(d$numeric), num_cols)
  expect_equal(nrow(d$numeric), 1L)
  expect_equal(nrow(d$categorical), 0L)             # empty, correct columns
  expect_equal(names(d$categorical), cat_cols)
  s <- d$numeric
  expect_equal(s$n, 5L)
  expect_equal(s$n_missing, 0L)
  expect_equal(s$mean, 3)
  expect_equal(s$median, 3)
  expect_equal(s$sd, sd(1:5))
  expect_equal(s$var, var(1:5))
  expect_equal(s$iqr, IQR(1:5))
  expect_equal(s$skewness, 0)                        # symmetric
  expect_equal(s$kurtosis, ds_kurtosis(1:5))
  se <- sd(1:5) / sqrt(5)                            # 95% CI uses t
  expect_equal(s$ci_lower, 3 - qt(0.975, df = 4) * se)
  expect_equal(s$ci_upper, 3 + qt(0.975, df = 4) * se)
})

test_that("describe_var() counts missing values", {
  d <- describe_var(c(1, 2, NA, 4))
  expect_equal(d$numeric$n, 3L)
  expect_equal(d$numeric$n_missing, 1L)
  expect_equal(d$numeric$mean, mean(c(1, 2, 4)))
})

test_that("describe_var() summarises one categorical variable", {
  d <- describe_var(factor(c("a", "a", "b")))
  expect_equal(nrow(d$numeric), 0L)
  expect_equal(names(d$categorical), cat_cols)
  expect_equal(d$categorical$category, c("a", "b"))  # nominal -> by count
  expect_equal(d$categorical$n, c(2L, 1L))
  expect_equal(d$categorical$rel, c(2 / 3, 1 / 3))   # proportion
  expect_equal(sum(d$categorical$rel), 1)
})

test_that("describe_var(frequencies = FALSE) gives one categorical summary row", {
  d <- describe_var(factor(c("a", "a", "b", "c")), frequencies = FALSE)
  expect_named(d$categorical,
    c("variable", "type", "n", "n_missing", "n_categories", "mode", "mode_rel"))
  expect_equal(nrow(d$categorical), 1L)
  expect_equal(d$categorical$type, "nominal")
  expect_equal(d$categorical$n, 4L)
  expect_equal(d$categorical$n_categories, 3L)
  expect_equal(d$categorical$mode, "a")
  expect_equal(d$categorical$mode_rel, 0.5)
})

test_that("describe_var() lumps rare nominal categories into '(other)'", {
  x <- factor(rep(letters[1:12], each = 2))
  d <- describe_var(x, max_categories = 5)                # frequencies TRUE
  expect_equal(nrow(d$categorical), 5L)                   # 4 kept + (other)
  expect_true("(other)" %in% d$categorical$category)
  expect_equal(sum(d$categorical$n), 24L)
  expect_equal(sum(d$categorical$rel), 1)
  # default is Inf -> no lumping
  expect_equal(nrow(describe_var(x)$categorical), 12L)
})

test_that("describe_var() never lumps ordinal variables and keeps level order", {
  x <- factor(rep(letters[1:6], each = 2),
    levels = letters[1:6], ordered = TRUE)
  d <- describe_var(x, max_categories = 3)
  expect_equal(nrow(d$categorical), 6L)                   # not lumped
  expect_equal(d$categorical$category, letters[1:6])      # level order kept
})

test_that("describe_var() emits identifier and type hints", {
  id  <- sprintf("id%02d", 1:10)                          # 10 distinct in 10
  m1  <- catch_messages(describe_var(id, xlab = "exp_id"))
  expect_match(m1, "'exp_id' looks like an identifier", all = FALSE)

  m2  <- catch_messages(describe_var(rep(1:3, 4), xlab = "code"))
  expect_match(m2, "'code' is stored as integer", all = FALSE)
})

test_that("describe_var() names the variable by the part after $ or xlab", {
  expect_equal(describe_var(iris$Sepal.Length)$numeric$variable, "Sepal.Length")
  expect_equal(describe_var(iris$Sepal.Length, xlab = "SL")$numeric$variable, "SL")
})

test_that("describe_var() groups numeric summaries with a leading column", {
  d <- describe_var(iris$Sepal.Length, by = iris$Species)
  expect_equal(names(d$numeric)[1], "Species")       # named after the variable
  expect_equal(nrow(d$numeric), 3L)                  # one row per group
  expect_equal(as.character(d$numeric$Species),
    c("setosa", "versicolor", "virginica"))
  expect_equal(d$numeric$mean[1],
    mean(iris$Sepal.Length[iris$Species == "setosa"]))
})

test_that("describe_var() accepts a named list of grouping variables", {
  d <- describe_var(iris$Sepal.Length, by = list(sp = iris$Species))
  expect_equal(names(d$numeric)[1], "sp")
})

test_that("describe_var() computes rel within each group", {
  g <- factor(c("x", "x", "x", "y", "y"))
  v <- factor(c("a", "a", "b", "a", "b"))
  d <- describe_var(v, by = g)
  # group x: a=2/3, b=1/3 ; group y: a=1/2, b=1/2 -> each group sums to 1
  by_grp <- tapply(d$categorical$rel, d$categorical[[1]], sum)
  expect_true(all(abs(by_grp - 1) < 1e-8))
})

test_that("describe_var() errors on a data frame and bad grouping", {
  expect_error(describe_var(iris), "describe_df")
  expect_error(describe_var(iris$Sepal.Length, by = iris$Petal.Length),
    "must be categorical")
  expect_error(describe_var(iris$Sepal.Length, by = factor(c("a", "b"))),
    "same length")
})

test_that("describe_var() print shows headings; str shows both elements", {
  expect_output(print(describe_var(iris$Sepal.Length)), "Numeric variables")
  expect_output(print(describe_var(iris$Species)), "Categorical variables")
  expect_output(print(describe_var(iris$Sepal.Length, language = "de")),
    "Numerische Variablen")
  expect_output(str(describe_var(iris$Sepal.Length)), "categorical")
})

test_that("describe_var() print hints how to see hidden columns, on stdout", {
  res <- describe_var(iris$Sepal.Length)        # 15 numeric columns

  # narrow console -> columns hidden -> hint shown (to stdout)
  withr::local_options(width = 80)
  narrow <- paste(capture.output(print(res)), collapse = "\n")
  expect_match(narrow, "print(<result>$numeric, width = Inf)", fixed = TRUE)

  # wide console -> everything fits -> no hint
  withr::local_options(width = 10000)
  wide <- paste(capture.output(print(res)), collapse = "\n")
  expect_false(grepl("width = Inf", wide, fixed = TRUE))
})

test_that("describe_var() column hint names the right component and language", {
  withr::local_options(width = 60)
  # numeric component
  out_num <- paste(capture.output(print(describe_var(iris$Sepal.Length))),
    collapse = "\n")
  expect_match(out_num, "View(<result>$numeric)", fixed = TRUE)
  # German wording
  out_de <- paste(capture.output(
    print(describe_var(iris$Sepal.Length, language = "de"))), collapse = "\n")
  expect_match(out_de, "View(<Ergebnis>$numeric)", fixed = TRUE)
})
