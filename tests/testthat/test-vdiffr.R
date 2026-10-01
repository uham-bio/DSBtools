# Optional visual regression tests. Visual snapshots are inherently sensitive to
# the graphics/font stack, so these run only when explicitly enabled with
# VDIFFR_RUN=true (e.g. VDIFFR_RUN=true Rscript -e 'devtools::test()'). They are
# skipped in the normal test run, under R CMD check, on CI, and on CRAN.

test_that("plots have a stable appearance", {
  skip_if(!identical(Sys.getenv("VDIFFR_RUN"), "true"),
          "Set VDIFFR_RUN=true to run visual regression tests")
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("vdiffr")

  p_cont <- with_null_device(
    suppressMessages(explore_var(iris$Sepal.Length, xlab = "Sepal length"))
  )
  vdiffr::expect_doppelganger("one-continuous", p_cont)

  p_nom <- with_null_device(
    suppressMessages(explore_var(iris$Species, xlab = "Species"))
  )
  vdiffr::expect_doppelganger("one-nominal", p_nom)

  p_catnum <- with_null_device(
    suppressMessages(explore_var(iris$Species, iris$Sepal.Length,
                                 xlab = "Species", ylab = "Sepal length"))
  )
  vdiffr::expect_doppelganger("categorical-by-numeric", p_catnum)
})
