test_that("explore_var() validates its arguments", {
  expect_error(
    explore_var(1:5, 1:4),
    "same length"
  )
  expect_error(
    explore_var(1:5, max_bars = 0),
    "max_bars"
  )
  expect_error(
    explore_var(1:5, max_bars = "many"),
    "max_bars"
  )
  expect_error(
    suppressMessages(explore_var(1:5, x_type = "nonsense")),
    "must be one of"
  )
})

test_that("explore_var() returns a patchwork object invisibly for every combination", {
  combos <- list(
    "continuous"       = quote(explore_var(iris$Sepal.Length, xlab = "SL")),
    "discrete"         = quote(explore_var(as.integer(InsectSprays$count), xlab = "n")),
    "nominal"          = quote(explore_var(iris$Species, xlab = "Sp")),
    "ordinal"          = quote(explore_var(esoph$agegp, xlab = "age")),
    "double->discrete" = quote(explore_var(InsectSprays$count, x_type = "discrete", xlab = "n")),
    "num x num"        = quote(explore_var(iris$Petal.Length, iris$Petal.Width, xlab = "PL", ylab = "PW")),
    "cat x num"        = quote(explore_var(ToothGrowth$dose, ToothGrowth$len, x_type = "ordinal", xlab = "d", ylab = "l")),
    "cat x cat"        = quote(explore_var(warpbreaks$tension, warpbreaks$wool, xlab = "T", ylab = "W")),
    "num x num + NA"   = quote(explore_var(airquality$Temp, airquality$Ozone, x_type = "continuous", y_type = "continuous", xlab = "T", ylab = "O"))
  )
  for (nm in names(combos)) {
    out <- with_null_device(suppressMessages(eval(combos[[nm]])))
    expect_s3_class(out, "patchwork")
  }
})

test_that("explore_var() returns invisibly", {
  with_null_device(
    suppressMessages(expect_invisible(explore_var(iris$Sepal.Length)))
  )
})

test_that("explore_var() derives default axis labels from the expression", {
  # strip_dollar() keeps only the part after "$", as in describe_var()
  msgs <- catch_messages(with_null_device(explore_var(iris$Sepal.Length)))
  expect_match(msgs, "Type of 'Sepal.Length'", all = FALSE)
})

test_that("explore_var() defaults language and max_bars to their options", {
  fmls <- formals(explore_var)
  expect_match(deparse(fmls$language), "DSBtools.language")
  expect_match(deparse(fmls$max_bars), "DSBtools.max_bars")
})

test_that("DSBtools.language option controls output language, argument overrides it", {
  withr::local_options(DSBtools.language = "de")
  msgs_de <- catch_messages(with_null_device(explore_var(iris$Sepal.Length, xlab = "SL")))
  expect_match(msgs_de, "Typ von 'SL'", all = FALSE)

  # explicit argument wins over the option
  msgs_en <- catch_messages(
    with_null_device(explore_var(iris$Sepal.Length, xlab = "SL", language = "en"))
  )
  expect_match(msgs_en, "Type of 'SL'", all = FALSE)
})

test_that("DSBtools.max_bars option and argument are both honoured", {
  # A discrete variable spanning 1:40 is a bar chart when max_bars is large and
  # a binned histogram when max_bars is small; both paths must run cleanly.
  x <- 1:40
  withr::local_options(DSBtools.max_bars = 50)
  out_bars <- with_null_device(suppressMessages(explore_var(x, x_type = "discrete", xlab = "x")))
  expect_s3_class(out_bars, "patchwork")

  # argument overrides the (large) option, forcing the histogram path
  out_hist <- with_null_device(
    suppressMessages(explore_var(x, x_type = "discrete", xlab = "x", max_bars = 10))
  )
  expect_s3_class(out_hist, "patchwork")
})
