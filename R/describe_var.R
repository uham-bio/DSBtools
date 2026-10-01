#' Describe a single variable
#'
#' @description
#' `describe_var()` summarizes one variable and returns the descriptive
#' statistics as tidy tables. The summary can optionally be grouped by one or
#' more categorical variables. To describe every column of a data frame, use
#' [describe_df()].
#'
#' @param x A vector (numeric, integer, character, logical, or factor). Data
#'   frames are not accepted; use [describe_df()] instead.
#' @param by A categorical vector of the same length as `x`, or a named list or
#'   data frame of several such vectors, to group the summary by; or `NULL`
#'   (default) for no grouping.
#' @param xlab A single character string used as the variable name in the
#'   `variable` column, or `NULL` (default) to use the expression passed as `x`
#'   (the part after `$`, if present).
#' @param x_type A single character string giving the type of `x`, one of
#'   `"nominal"`, `"ordinal"`, `"discrete"`, or `"continuous"` (partial matching
#'   allowed), or `NULL` (default) to detect the type automatically (see
#'   [explore_var()]).
#' @param frequencies A single logical value. If `TRUE` (default), a
#'   categorical variable is summarized as a frequency table in long format; if
#'   `FALSE`, in one summary row.
#' @param max_categories A single positive number, used only if
#'   `frequencies = TRUE`. Nominal variables with more categories are reduced to
#'   the most frequent ones, and the remaining categories are combined into one
#'   row. Ordinal variables are never combined. Defaults to `Inf` (no
#'   combining).
#' @param language A single character string, either `"en"` or `"de"`. Controls
#'   the language of the console hints and the printed headings. Defaults to
#'   `getOption("DSBtools.language", "en")`.
#'
#' @details
#' Depending on the type of `x`, either the `numeric` or the `categorical`
#' table is filled; the other table has zero rows. The type is detected as in
#' [explore_var()] unless it is set via `x_type`.
#'
#' If `by` is used, each grouping variable appears as its own leading column,
#' named after the variable (as with `dplyr::group_by()` and
#' `dplyr::summarize()`), with one row per group combination. Grouping
#' variables must be categorical; otherwise, an informative error is raised.
#'
#' The 95% confidence interval of the mean is always based on the
#' *t*-distribution. `describe_var()` does not perform statistical tests.
#'
#' While summarizing, `describe_var()` prints hints to the console if `x` may
#' need a closer look:
#' * a categorical variable that looks like an identifier (at least half of the
#'   non-missing values are distinct),
#' * an integer variable with few distinct values (possible category codes),
#' * a `double` variable containing only whole numbers (possible counts).
#'
#' @section Skewness and kurtosis:
#' Skewness (\eqn{g_1}) and excess kurtosis (\eqn{g_2}) are calculated as
#' moment coefficients:
#' \deqn{g_1 = \frac{m_3}{m_2^{3/2}}, \qquad g_2 = \frac{m_4}{m_2^{2}} - 3,
#'   \qquad m_k = \frac{1}{n}\sum_{i=1}^{n}(x_i - \bar{x})^k}
#' The central moments \eqn{m_k} use \eqn{1/n} (not \eqn{n - 1} as in
#' [stats::sd()]). For a normal distribution, both \eqn{g_1} and \eqn{g_2} are
#' 0. Positive values of \eqn{g_1} indicate right skew, negative values left
#' skew; positive values of \eqn{g_2} indicate heavier tails (more extreme
#' values) than in a normal distribution, negative values lighter tails. As a
#' rule of thumb, \eqn{|g_1| < 0.5} indicates a roughly symmetric, \eqn{0.5}
#' to \eqn{1} a moderately skewed, and \eqn{> 1} a strongly skewed
#' distribution (Bulmer, 1979).
#'
#' Skewness is `NA` for fewer than 3 non-missing values, kurtosis for fewer
#' than 4, and both if the variance is zero.
#'
#' @return
#' The function returns an object of class `"ds_description"`, which is a named
#' list including the following two tibbles (displayed by its print method):
#' \describe{
#'   \item{`numeric`}{For a numeric variable, one row (per group combination if
#'     `by` is used) with the sample size (`n`, `n_missing`), location (`mean`,
#'     `median`), spread (`sd`, `var`, `iqr`, `min`, `max`), shape (`skewness`,
#'     `kurtosis`), and inference (`se`, `ci_lower`, `ci_upper`).}
#'   \item{`categorical`}{For a categorical variable and `frequencies = TRUE`,
#'     one row per category with `variable`, `category`, `n`, and the
#'     proportion `rel` (0–1; within each group if `by` is used). For
#'     `frequencies = FALSE`, one row with `variable`, `type`, `n`,
#'     `n_missing`, `n_categories`, and the most frequent category (`mode`)
#'     with its proportion (`mode_rel`).}
#' }
#' All values are numeric and unrounded. Column names are always English,
#' independent of `language`.
#'
#' @references
#' Bulmer, M. G. (1979). *Principles of Statistics*. Dover Publications,
#' New York.
#'
#' Joanes, D. N., & Gill, C. A. (1998). Comparing measures of sample skewness
#' and kurtosis. *Journal of the Royal Statistical Society: Series D (The
#' Statistician)*, 47(1), 183–189. \doi{10.1111/1467-9884.00122}
#'
#' @seealso [explore_var()] for visualizing a single variable.
#' @family data description functions
#'
#' @export
#'
#' @examples
#' # one numeric variable
#' describe_var(iris$Sepal.Length)
#'
#' # grouped by a categorical variable
#' describe_var(iris$Sepal.Length, by = iris$Species)
#'
#' # one categorical variable (frequency table)
#' describe_var(iris$Species)
#'
#' # month is stored as integer but is an ordered category
#' describe_var(airquality$Month, x_type = "ordinal")
#'
#' # extract the numbers for further use
#' res <- describe_var(iris$Sepal.Length, by = iris$Species)
#' res$numeric[, c("Species", "mean", "sd")]
describe_var <- function(x, by = NULL, x_type = NULL, xlab = NULL,
  frequencies = TRUE, max_categories = Inf,
  language = getOption("DSBtools.language", "en")) {

  if (is.data.frame(x))
    stop("`x` is a data frame. Use describe_df() to summarise every column.",
      call. = FALSE)
  language <- match.arg(language, c("en", "de"))
  xname    <- xlab %||% strip_dollar(deparse1(substitute(x)))

  x_type <- norm_type(x_type, "x_type")
  if (is.null(x_type)) {
    xtype <- detect_type(x)
    xv    <- x
  } else {
    xtype <- x_type
    xv    <- suppressMessages(apply_type(x, x_type, xname, ev_txt("en")))
  }

  ds_emit_hints(x, xname, xtype, language, "[describe_var]",
    type_hints = is.null(x_type))

  by_list <- normalize_by(by, substitute(by))
  grp_df  <- if (is.null(by_list)) NULL else ds_grp_df(by_list, length(x))
  other   <- dv_txt(language)$other

  if (xtype %in% c("nominal", "ordinal")) {
    res <- list(
      numeric     = ds_empty_numeric(grp_df),
      categorical = ds_build_categorical(xv, xname, xtype, grp_df,
        frequencies, max_categories, other))
  } else {
    res <- list(
      numeric     = ds_build_numeric(as.numeric(xv), xname, grp_df),
      categorical = ds_empty_categorical(grp_df, frequencies))
  }
  new_ds_description(res, language)
}

#' @export
print.ds_description <- function(x, ...) {
  txt   <- dv_txt(attr(x, "language") %||% "en")
  shown <- FALSE
  # cli's cat_* family writes to stdout, so the headings survive `message =
  # FALSE` (e.g. in a knitr chunk); the tibbles print to stdout anyway. The
  # tibbles use the default print width (so the output matches printing
  # $numeric / $categorical directly); a hint is added when columns are hidden.
  if (nrow(x$numeric) > 0L) {
    cli::cat_rule(txt$num)
    print(x$numeric)
    ds_emit_cols_hint(x$numeric, "$numeric", txt)
    shown <- TRUE
  }
  if (nrow(x$categorical) > 0L) {
    if (shown) cli::cat_line()
    cli::cat_rule(txt$cat)
    print(x$categorical)
    ds_emit_cols_hint(x$categorical, "$categorical", txt)
    shown <- TRUE
  }
  if (!shown) cli::cat_line("<describe: no data>")
  invisible(x)
}
