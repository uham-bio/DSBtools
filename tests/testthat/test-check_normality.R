# Suppress the once-per-session reading guide by default; the guide test resets
# it. (The guide is printed via cli, not as a message.)
.ds_env$normality_guide_shown <- TRUE

test_that("check_normality() returns a patchwork object invisibly", {
  with_null_device(
    suppressMessages(expect_invisible(check_normality(iris$Sepal.Length)))
  )
  out <- with_null_device(suppressMessages(check_normality(iris$Sepal.Length)))
  expect_s3_class(out, "patchwork")
})

test_that("check_normality() prints a one-line header with N and missing", {
  msgs <- catch_messages(with_null_device(
    check_normality(iris$Sepal.Length, xlab = "SL")))
  expect_match(msgs, "'SL': N = 150 \\(0 missing\\)", all = FALSE)
})

test_that("check_normality() does not print the Shapiro-Wilk result", {
  msgs <- catch_messages(with_null_device(check_normality(iris$Sepal.Length)))
  expect_false(any(grepl("Shapiro", msgs)))
})

test_that("check_normality() shows the reading guide once per session", {
  .ds_env$normality_guide_shown <- NULL                     # reset
  first <- paste(cli::cli_fmt(suppressMessages(
    with_null_device(check_normality(iris$Sepal.Length)))), collapse = "\n")
  expect_match(first, "How to read the plots:")             # translated heading
  expect_match(first, "Q-Q plot")                           # bullet
  expect_match(first, "once per session")                   # translated footer
  # second call in the same session: the guide is not shown again
  second <- paste(cli::cli_fmt(suppressMessages(
    with_null_device(check_normality(iris$Sepal.Length)))), collapse = "\n")
  expect_false(grepl("How to read the plots", second))
})

test_that("check_normality() translates the guide heading and footer", {
  .ds_env$normality_guide_shown <- NULL
  out <- paste(cli::cli_fmt(suppressMessages(with_null_device(
    check_normality(iris$Sepal.Length, language = "de")))), collapse = "\n")
  expect_match(out, "So liest du die Grafiken:")
  expect_match(out, "einmal pro Sitzung")
  .ds_env$normality_guide_shown <- TRUE
})

test_that("check_normality() hints at Q-Q steps for discrete integers", {
  x    <- as.integer(rep(1:4, 10))
  msgs <- catch_messages(with_null_device(check_normality(x, xlab = "counts")))
  expect_match(msgs, "steps in the Q-Q", all = FALSE)
})

test_that("check_normality() errors informatively on zero variance", {
  expect_error(check_normality(rep(5, 10)), "zero variance")
  expect_error(check_normality(rep(5, 10), language = "de"), "keine Varianz")
})

test_that("check_normality() validates its input", {
  expect_error(check_normality(c("a", "b", "c")), "must be numeric")
  expect_error(suppressMessages(check_normality(c(1, NA, NA))),
    "at least 3 non-missing")
})
