# Column names used inside ggplot2::aes() in explore_var().
# Declared here to avoid "no visible binding for global variable" NOTEs
# in R CMD check. globalVariables() is imported from utils (see the
# @importFrom in R/explore_var.R).
globalVariables(c(
  "x", "y", "g", "v", "mw", "lo", "hi", "count", "density"
))
