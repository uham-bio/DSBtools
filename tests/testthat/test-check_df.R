test_that("cd_is_codes() flags few-valued whole numbers", {
  expect_true(cd_is_codes(c(1L, 2L, 1L, 2L)))
  expect_true(cd_is_codes(c(1, 2, 3)))            # whole doubles, few distinct
  expect_false(cd_is_codes(1:50))                 # many distinct
  expect_false(cd_is_codes(c(1.5, 2.5)))          # not whole
  expect_false(cd_is_codes(c("a", "b")))          # not numeric
})

test_that("cd_is_count_double() flags whole doubles with many distinct values", {
  expect_true(cd_is_count_double(as.double(1:20)))
  expect_false(cd_is_count_double(1:20))          # integer storage, not double
  expect_false(cd_is_count_double(c(1, 2, 3)))    # too few distinct
})

test_that("cd_num_as_text() flags numeric-looking text", {
  expect_true(cd_num_as_text(c("1", "2", "3")))
  expect_true(cd_num_as_text(factor(c("10", "20"))))
  expect_false(cd_num_as_text(c("a", "b")))
  expect_false(cd_num_as_text(c("1", "x")))
  expect_false(cd_num_as_text(1:3))               # already numeric
})

test_that("cd_spelling_variants() collapses case/whitespace variants", {
  v <- c("North", "north ", "South", "SOUTH", "North")
  res <- cd_spelling_variants(v)
  expect_length(res, 2)
  expect_true(any(vapply(res, function(g) setequal(g, c("North", "north ")), logical(1))))
  expect_length(cd_spelling_variants(c("a", "b", "c")), 0)
})

test_that("cd_outliers() counts values beyond the 1.5*IQR fences", {
  ol <- cd_outliers(c(1:10, 100))
  expect_gte(ol$n, 1)
  expect_true(100 %in% ol$values)
  # not enough distinct values -> no check
  expect_equal(cd_outliers(c(1, 1, 2, 2))$n, 0)
  expect_equal(cd_outliers(c("a", "b"))$n, 0)
})

messy <- data.frame(
  region = c("North", "north ", "South", "SOUTH", "North"),
  n_obs  = c("12", "9", "15", "7", "20"),
  group  = c(1L, 2L, 1L, 2L, 1L),
  stringsAsFactors = FALSE
)

test_that("check_df() returns a classed list of two tibbles (no plot)", {
  out <- check_df(messy)
  expect_s3_class(out, "check_df")
  expect_named(out, c("overview", "issues"))
  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$issues, "tbl_df")
  expect_false(inherits(out, "gg"))          # not a plot
})

test_that("check_df() overview has fixed English columns and English types", {
  out <- check_df(messy)$overview
  expect_named(out, c("variable", "type", "n_missing", "n_distinct", "n_issues"))
  expect_equal(out$type, c("nominal", "nominal", "discrete"))
  expect_type(out$n_missing, "integer")
  expect_type(out$n_distinct, "integer")
  expect_equal(out$n_issues, c(1L, 1L, 1L))  # one issue per column here
})

test_that("check_df() issues uses fixed English codes with translated hints", {
  iss <- check_df(messy)$issues
  expect_named(iss, c("variable", "issue", "hint"))
  expect_setequal(iss$issue,
    c("spelling_variants", "numbers_as_text", "category_codes"))
  # hint (English by default) carries a concrete suggestion
  code_hint <- iss$hint[iss$issue == "category_codes"]
  expect_match(code_hint, "factor\\(\\)")
})

test_that("check_df() issues is a 0-row tibble when nothing is flagged", {
  out <- check_df(data.frame(a = c(1.5, 2.5, 3.5)))
  expect_equal(nrow(out$issues), 0L)
  expect_named(out$issues, c("variable", "issue", "hint"))
})

test_that("check_df() translates hints but keeps codes English", {
  iss <- check_df(data.frame(g = c(1L, 2L, 1L, 2L)), language = "de")$issues
  expect_equal(iss$issue, "category_codes")             # code stays English
  expect_match(iss$hint, "Codes f")                     # hint translated (German)
})

test_that("check_df() print shows heading and hint lines", {
  # the print method writes everything to stdout, so capture.output() alone
  # captures both the cli rules/bullets and the tibbles
  out <- paste(capture.output(print(check_df(messy))), collapse = "\n")
  expect_match(out, "Data check: 3 variables, 5 rows")
  expect_match(out, "Hints \\(3\\)")
  expect_match(out, "region:")

  clean <- paste(capture.output(
    print(check_df(data.frame(a = c(1.5, 2.5, 3.5))))), collapse = "\n")
  expect_match(clean, "No typical issues found")
})

test_that("check_df() print returns its input invisibly", {
  x   <- check_df(messy)
  ret <- NULL
  capture.output(ret <- withVisible(print(x)))
  expect_false(ret$visible)
  expect_identical(ret$value, x)
})

test_that("check_df() has no plot argument", {
  expect_error(check_df(messy, plot = FALSE), "unused argument")
})

test_that("check_df() validates code_threshold", {
  expect_error(check_df(messy, code_threshold = 0), "positive number")
  expect_error(check_df(messy, code_threshold = c(5, 10)), "positive number")
  expect_error(check_df(messy, code_threshold = "10"), "positive number")
})

test_that("check_df() validates its input", {
  expect_error(check_df(1:10), "data frame")
  expect_error(check_df(data.frame()[, FALSE]), "no columns")
})
