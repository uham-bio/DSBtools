txt_en <- ev_txt("en")
txt_de <- ev_txt("de")

test_that("resolve_var() reports the detected type", {
  msgs <- catch_messages(resolve_var(iris$Species, NULL, "Species", "x_type", txt_en))
  expect_match(msgs, "Type of 'Species': nominal \\(detected automatically\\)", all = FALSE)
})

test_that("resolve_var() reports a specified type", {
  msgs <- catch_messages(resolve_var(mtcars$am, "nominal", "am", "x_type", txt_en))
  expect_match(msgs, "Type of 'am': nominal \\(specified\\)", all = FALSE)
})

test_that("resolve_var() hints at category codes for small-range integers", {
  msgs <- catch_messages(resolve_var(1:5, NULL, "v", "x_type", txt_en))
  expect_match(msgs, "stored as integer and has only 5 distinct", all = FALSE)
})

test_that("resolve_var() does not hint at codes for wide-range integers", {
  msgs <- catch_messages(resolve_var(1:50, NULL, "v", "x_type", txt_en))
  expect_false(any(grepl("distinct", msgs)))
})

test_that("resolve_var() hints at counts for whole-number doubles", {
  msgs <- catch_messages(resolve_var(c(1, 2, 3, 4), NULL, "v", "x_type", txt_en))
  expect_match(msgs, "contains only whole numbers", all = FALSE)
})

test_that("resolve_var() does not hint at counts for genuine doubles", {
  msgs <- catch_messages(resolve_var(c(1.5, 2.5), NULL, "v", "x_type", txt_en))
  expect_false(any(grepl("whole numbers", msgs)))
})

test_that("messages are localised to German", {
  msgs <- catch_messages(resolve_var(iris$Species, NULL, "Art", "x_type", txt_de))
  expect_match(msgs, "Typ von 'Art': nominal \\(automatisch erkannt\\)", all = FALSE)

  msgs2 <- catch_messages(resolve_var(1:5, NULL, "v", "x_type", txt_de))
  expect_match(msgs2, "als integer gespeichert", all = FALSE)
})

test_that("explore_var() emits the type message end-to-end", {
  with_null_device(
    expect_message(
      explore_var(iris$Sepal.Length, xlab = "SL"),
      "Type of 'SL': continuous"
    )
  )
})
