test_that("explore_df() returns one plot per column", {
  out <- with_null_device(suppressMessages(explore_df(iris)))
  expect_type(out, "list")
  expect_named(out, names(iris))
  expect_true(all(vapply(out, function(p) inherits(p, "patchwork"), logical(1))))
})

test_that("explore_df() with a target skips the target column", {
  out <- with_null_device(suppressMessages(explore_df(iris, target = "Species")))
  expect_named(out, setdiff(names(iris), "Species"))
  expect_length(out, ncol(iris) - 1L)
})

test_that("explore_df() returns invisibly", {
  with_null_device(
    suppressMessages(expect_invisible(explore_df(iris[, 1:2])))
  )
})

test_that("explore_df() validates its input", {
  expect_error(explore_df(1:10), "data frame")
  expect_error(explore_df(iris, target = "nope"), "not found")
  expect_error(explore_df(iris, target = c("a", "b")), "single column name")
})

test_that("explore_df() keeps going when one column fails", {
  # an all-NA logical column cannot be plotted; the failure must not stop the
  # other columns from being processed.
  df <- data.frame(ok = 1:5, bad = rep(NA, 5))
  out <- suppressWarnings(with_null_device(suppressMessages(explore_df(df))))
  expect_named(out, c("ok", "bad"))
  expect_true(inherits(out$ok, "patchwork"))
  expect_null(out$bad)
})

test_that("explore_df() prints one intro line and one bullet per column", {
  msgs <- catch_messages(with_null_device(
    explore_df(airquality[, c("Ozone", "Temp", "Wind")])))
  one <- paste(msgs, collapse = "\n")
  expect_match(one, "\\[explore_df\\] Plotting 3 variable")
  expect_match(one, "- Ozone: discrete", fixed = TRUE)
  expect_match(one, "- Wind: continuous", fixed = TRUE)
  # explore_var()'s own per-call type message is suppressed here
  expect_false(any(grepl("Type of", msgs)))
})

test_that("explore_df() hint for ambiguous columns suggests encoding, not x_type", {
  msgs <- catch_messages(with_null_device(
    explore_df(airquality[, c("Ozone", "Month")])))
  one <- paste(msgs, collapse = "\n")
  expect_match(one, "- Month: discrete", fixed = TRUE)
  expect_match(one, "encode the column before calling explore_df()", fixed = TRUE)
  expect_false(any(grepl("x_type", msgs)))     # x_type is not an argument here
})

test_that("explore_df() reports a failed column without stopping", {
  df   <- data.frame(ok = 1:5, bad = rep(NA, 5))
  msgs <- suppressWarnings(catch_messages(with_null_device(explore_df(df))))
  one  <- paste(msgs, collapse = "\n")
  expect_match(one, "- ok: discrete", fixed = TRUE)
  expect_match(one, "could not be plotted", fixed = TRUE)
})

test_that("explore_df() translates the compact console output", {
  msgs <- catch_messages(with_null_device(
    explore_df(airquality[, c("Ozone", "Wind")], language = "de")))
  one <- paste(msgs, collapse = "\n")
  expect_match(one, "werden dargestellt", fixed = TRUE)
  expect_match(one, "- Wind: kontinuierlich", fixed = TRUE)
})
