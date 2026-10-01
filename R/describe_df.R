#' Describe every column of a data frame
#'
#' @description
#' `describe_df()` summarizes every column of a data frame separately and
#' returns the descriptive statistics as tidy tables. The summaries can
#' optionally be grouped by one or more categorical variables.
#'
#' @param data A data frame.
#' @param by A character vector with the names of one or more categorical columns
#'   of `data` to group the summaries by, or `NULL` (default) for no grouping.
#' @param frequencies A single logical value. If `TRUE`, the full frequency table
#'   is returned in long format; if `FALSE`, each categorical variable is
#'   summarized in one row. Defaults to `FALSE`.
#' @param max_categories A single positive number, used only if
#'   `frequencies = TRUE`. Nominal variables with more categories are reduced to
#'   the most frequent ones, and the remaining categories are combined into one
#'   row. Defaults to `getOption("DSBtools.max_categories", 10)`.
#' @param language A single character string, either `"en"` or `"de"`. Controls
#'   the language of the console hints and the printed headings. Defaults to
#'   `getOption("DSBtools.language", "en")`.
#'
#' @details
#' Each column of `data` is treated as one variable, and its type is detected
#' as in [explore_var()]. Numeric variables are summarized in the `numeric`
#' table, categorical variables in the `categorical` table.
#'
#' The columns named in `by` are used for grouping only and are not summarized
#' themselves. Each grouping variable appears as a leading column in both
#' tables. Grouping variables must be categorical; otherwise, an informative
#' error is raised.
#'
#' While summarizing, `describe_df()` prints hints to the console for columns
#' that may need a closer look:
#' * categorical columns that look like identifiers (e.g., one distinct value
#'   per row); these are still summarized,
#' * integer columns with few distinct values (possible category codes),
#' * `double` columns containing only whole numbers (possible counts).
#'
#' @inheritSection describe_var Skewness and kurtosis
#'
#' @return
#' The function returns an object of class `"ds_description"`, which is a named
#' list including the following two tibbles (displayed by its print method):
#' \describe{
#'   \item{`numeric`}{One row per numeric variable (and group combination if
#'     `by` is used), with the sample size (`n`, `n_missing`), location (`mean`,
#'     `median`), spread (`sd`, `var`, `iqr`, `min`, `max`), shape (`skewness`,
#'     `kurtosis`), and inference (`se`, `ci_lower`, `ci_upper`). All values
#'     are numeric and unrounded.}
#'   \item{`categorical`}{If `frequencies = FALSE`, one row per categorical
#'     variable with `variable`, `type`, `n`, `n_missing`, `n_categories`, and
#'     the most frequent category (`mode`) with its proportion (`mode_rel`). If
#'     `frequencies = TRUE`, one row per category with `variable`, `category`,
#'     `n`, and the proportion `rel` (0–1; within each group if `by` is used).}
#' }
#' Tables that do not apply have zero rows. Column names are always English,
#' independent of `language`.
#'
#' @references
#' Joanes, D. N., & Gill, C. A. (1998). Comparing measures of sample skewness
#' and kurtosis. *Journal of the Royal Statistical Society: Series D (The
#' Statistician)*, 47(1), 183–189. \doi{10.1111/1467-9884.00122}
#'
#' @seealso [explore_df()] for visualizing every column of a data frame.
#' @family data description functions
#'
#' @export
#'
#' @examples
#' describe_df(iris)
#'
#' # summarize every variable within each species
#' describe_df(iris, by = "Species")
#'
#' # full frequency tables for categorical variables
#' describe_df(warpbreaks, frequencies = TRUE)
#'
#' # the results are tibbles and can be used further
#' res <- describe_df(iris, by = "Species")
#' res$numeric[, c("Species", "variable", "mean", "sd")]
describe_df <- function(data, by = NULL, frequencies = FALSE,
  max_categories = getOption("DSBtools.max_categories", 10),
  language = getOption("DSBtools.language", "en")) {

  if (!is.data.frame(data))
    stop("`data` must be a data frame.", call. = FALSE)
  language <- match.arg(language, c("en", "de"))

  by <- by %||% character(0)
  if (!is.character(by))
    stop("`by` must be a character vector of column names.", call. = FALSE)
  notfound <- setdiff(by, names(data))
  if (length(notfound))
    stop(sprintf("`by` column(s) not found in `data`: %s",
      paste(notfound, collapse = ", ")), call. = FALSE)

  grp_df <- if (length(by)) data[by] else NULL
  if (!is.null(grp_df)) {
    for (nm in by) {
      typ <- detect_type(data[[nm]])
      if (!typ %in% c("nominal", "ordinal"))
        stop(sprintf(
          "grouping variable '%s' must be categorical (nominal or ordinal), not %s.",
          nm, typ), call. = FALSE)
    }
  }

  vars      <- setdiff(names(data), by)
  other     <- dv_txt(language)$other
  num_parts <- list()
  cat_parts <- list()
  for (nm in vars) {
    v   <- data[[nm]]
    typ <- detect_type(v)
    ds_emit_hints(v, nm, typ, language, "[describe_df]")
    if (typ %in% c("nominal", "ordinal"))
      cat_parts[[nm]] <- ds_build_categorical(v, nm, typ, grp_df, frequencies,
        max_categories, other)
    else
      num_parts[[nm]] <- ds_build_numeric(as.numeric(v), nm, grp_df)
  }

  num_tbl <- if (length(num_parts))
    tibble::as_tibble(do.call(rbind, num_parts)) else ds_empty_numeric(grp_df)
  cat_tbl <- if (length(cat_parts))
    tibble::as_tibble(do.call(rbind, cat_parts))
  else ds_empty_categorical(grp_df, frequencies)

  new_ds_description(list(numeric = num_tbl, categorical = cat_tbl), language)
}
