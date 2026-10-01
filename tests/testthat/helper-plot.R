# Evaluate `code` while a null PDF device is open, so that the print() side
# effect of explore_var() does not open a real graphics device or leave a
# stray Rplots.pdf during testing. Returns the value of `code` (invisibly if
# `code` itself is invisible, so expect_invisible() still works).
with_null_device <- function(code) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(code)
}
