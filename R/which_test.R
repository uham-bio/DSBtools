#' Suggest a suitable statistical test
#'
#' @description
#' `which_test()` looks at the types (scales of measurement) of two variables
#' and the number of groups, and suggests a suitable statistical test, together
#' with the reasoning, the assumptions to check, and a non-parametric
#' alternative. It is meant as a bridge from exploratory analysis (Data Science
#' 1) to inferential statistics (Data Science 2): it does **not** run the test,
#' it points you to the right one and shows the R function to call.
#'
#' @details
#' The suggestion follows the usual decision scheme:
#'
#' * **numeric vs numeric** -> correlation / linear regression (Pearson;
#'   Spearman if a variable is ordinal or clearly non-normal),
#' * **categorical vs numeric** -> comparison of group means: a *t*-test for two
#'   groups, one-way ANOVA for three or more (rank-based alternatives:
#'   Mann-Whitney U and Kruskal-Wallis),
#' * **categorical vs categorical** -> chi-squared test of independence
#'   (Fisher's exact test when expected counts are small).
#'
#' When a variable is ordinal, a rank-based test is suggested. These are
#' guidelines to support a decision, not a substitute for checking the
#' assumptions yourself (see [check_normality()]).
#'
#' @param x,y Two vectors of the same length.
#' @param language One of `"en"` or `"de"`. Controls the language of the
#'   printed explanation. Defaults to `getOption("DSBtools.language", "en")`.
#' @param xlab,ylab Optional variable names used in the printed R calls. Default
#'   to the deparsed expressions passed as `x` and `y`.
#'
#' @return An object of class `"ds_test_suggestion"` (a list with the detected
#'   types, scenario, group count, recommended test, and alternative), returned
#'   invisibly by its own print method. Printing it shows the full explanation.
#'
#' @seealso [explore_var()], [check_normality()]
#'
#' @examples
#' # categorical (2 groups) vs numeric -> t-test
#' which_test(sleep$group, sleep$extra)
#'
#' # categorical (3 groups) vs numeric -> ANOVA
#' which_test(iris$Species, iris$Sepal.Length)
#'
#' # numeric vs numeric -> correlation
#' which_test(iris$Petal.Length, iris$Petal.Width)
#'
#' # categorical vs categorical -> chi-squared / Fisher
#' which_test(warpbreaks$wool, warpbreaks$tension)
#'
#' @keywords internal
#' @noRd
which_test <- function(x, y,
  language = getOption("DSBtools.language", "en"),
  xlab = NULL, ylab = NULL) {

  language <- match.arg(language, c("en", "de"))
  if (length(x) != length(y))
    stop("`x` and `y` must have the same length.", call. = FALSE)

  xname <- xlab %||% deparse1(substitute(x))
  yname <- ylab %||% deparse1(substitute(y))

  sug          <- wt_suggest(x, y)
  sug$language  <- language
  sug$xname     <- xname
  sug$yname     <- yname
  class(sug)    <- "ds_test_suggestion"
  sug
}

# S3 print method for which_test() results. which_test() is currently internal
# (not exported); this method is registered in .onLoad() so printing still works.
#' @noRd
print.ds_test_suggestion <- function(x, ...) {
  txt <- wt_txt(x$language)

  cat(txt$hdr, "\n")
  cat(sprintf(txt$lbl_types,
    ds_type_label(x$x_type, x$language),
    ds_type_label(x$y_type, x$language)), "\n")

  # scenario line + reason
  reason <- switch(x$scenario,
    assoc_num = txt$reason_assoc,
    groups    = if (x$test == "none_one_group") txt$note_one_group
                else sprintf(txt$reason_groups, x$n_groups),
    cat_cat   = txt$reason_catcat
  )
  cat(txt$lbl_scenario, reason, "\n")

  if (identical(x$test, "none_one_group")) {
    cat("\n", txt$note_one_group, "\n", sep = "")
    return(invisible(x))
  }

  ti <- txt$tests[[x$test]]
  cat("\n", sprintf(txt$lbl_test, ti$name), "\n", sep = "")
  cat("   ", ti$call, "\n", sep = "")

  cat("\n", txt$lbl_assump, "\n", sep = "")
  for (a in ti$assump) cat("   - ", a, "\n", sep = "")

  # contextual notes
  notes <- character(0)
  if (isTRUE(x$cat_ordinal) || x$x_type == "ordinal" || x$y_type == "ordinal")
    notes <- c(notes, txt$note_ordinal)
  if (isTRUE(x$small_expected))
    notes <- c(notes, txt$note_small_expected)
  if (length(notes)) {
    cat("\n")
    for (nte in notes) cat("   ", nte, "\n", sep = "")
  }

  if (!is.null(x$alt) && !is.na(x$alt)) {
    alt <- txt$tests[[x$alt]]
    cat("\n", sprintf(txt$lbl_alt, alt$name), "\n", sep = "")
    cat("   ", alt$call, "\n", sep = "")
  }

  invisible(x)
}
