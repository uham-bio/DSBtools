#' Check a data frame for common data problems
#'
#' @description
#' `check_df()` gives a quick overview of a data frame before any analysis.
#' For every column, it reports the detected type (scale of measurement), the
#' number of missing values, and the number of distinct values, and it flags
#' typical beginner pitfalls with a concrete suggestion.
#'
#' @param data A data frame.
#' @param language A single character string, either `"en"` or `"de"`. Controls
#'   the language of the `hint` texts and the printed headings. Defaults to
#'   `getOption("DSBtools.language", "en")`.
#' @param code_threshold A single positive number. A numeric column of whole
#'   numbers with at most this many distinct values is flagged as possible
#'   category codes; a `double` column of whole numbers with more distinct values
#'   is flagged as possible counts. Defaults to `10`.
#'
#' @details
#' Types are detected exactly as in [explore_var()], from how each column is
#' stored in R (character/logical/unordered factor -> nominal, ordered factor
#' -> ordinal, integer -> discrete, double -> continuous).
#'
#' The following pitfalls are flagged:
#' * numbers that are really **codes for categories** (e.g., `1` = control,
#'   `2` = treatment),
#' * **counts stored as `double`** instead of integer,
#' * **spelling variants** of the same category (`"North"` / `"north "`),
#' * **numbers stored as text**,
#' * **outliers** (values outside 1.5 x IQR).
#'
#' These checks are heuristics meant to prompt a second look, not automatic
#' corrections; the data are never modified. Column names and issue codes in
#' the result are always English; only the `hint` texts and the printed
#' headings are translated.
#'
#' `check_df()` is the natural first step before [explore_var()] or
#' [explore_df()], which then show single variables in detail.
#'
#' @return
#' The function returns an object of class `"check_df"`, which is a named list
#' including the following two tibbles:
#' \describe{
#'   \item{`overview`}{One row per column of `data`, with the column name
#'     (`variable`), the detected type (`type`), the number of missing values
#'     (`n_missing`), the number of distinct non-missing values (`n_distinct`),
#'     and the number of flagged issues (`n_issues`).}
#'   \item{`issues`}{One row per detected issue, with the column name
#'     (`variable`), a fixed English issue code (`issue`; one of
#'     `"category_codes"`, `"counts_as_double"`, `"spelling_variants"`,
#'     `"numbers_as_text"`, or `"outliers"`), and the translated explanation
#'     with a concrete suggestion (`hint`). The tibble has zero rows if no
#'     issue is flagged.}
#' }
#' The object is returned visibly and displayed by its print method.
#'
#' @seealso [explore_var()] and [explore_df()] for exploring single variables
#'   in detail.
#' @family data description functions
#'
#' @importFrom stats quantile
#' @export
#'
#' @examples
#' check_df(airquality)
#'
#' # a small messy dataset
#' messy <- data.frame(
#'   region = c("North", "north ", "South", "SOUTH", "North"),
#'   n_obs  = c("12", "9", "15", "7", "20"),
#'   group  = c(1L, 2L, 1L, 2L, 1L)
#' )
#' res <- check_df(messy)
#' res$issues   # the flagged issues as data
check_df <- function(data,
  language       = getOption("DSBtools.language", "en"),
  code_threshold = 10) {

  language <- match.arg(language, c("en", "de"))
  if (!is.data.frame(data))
    stop("`data` must be a data frame.", call. = FALSE)
  if (ncol(data) == 0L)
    stop("`data` has no columns.", call. = FALSE)
  if (!is.numeric(code_threshold) || length(code_threshold) != 1L ||
      is.na(code_threshold) || code_threshold < 1)
    stop("`code_threshold` must be a single positive number.", call. = FALSE)

  txt <- cd_txt(language)

  ov_rows  <- vector("list", ncol(data))
  iss_rows <- list()
  for (i in seq_along(data)) {
    nm         <- names(data)[i]
    v          <- data[[nm]]
    col_issues <- cd_column_issues(v, txt, code_threshold)
    ov_rows[[i]] <- data.frame(
      variable   = nm,
      type       = detect_type(v),
      n_missing  = sum(is.na(v)),
      n_distinct = n_distinct(v),
      n_issues   = length(col_issues),
      stringsAsFactors = FALSE)
    for (iss in col_issues)
      iss_rows[[length(iss_rows) + 1L]] <- data.frame(
        variable = nm, issue = iss$issue, hint = iss$hint,
        stringsAsFactors = FALSE)
  }

  overview <- tibble::as_tibble(do.call(rbind, ov_rows))
  issues   <- if (length(iss_rows))
    tibble::as_tibble(do.call(rbind, iss_rows))
  else
    tibble::tibble(variable = character(0), issue = character(0),
      hint = character(0))

  structure(list(overview = overview, issues = issues),
    class = "check_df", language = language, n_row = nrow(data))
}

#' @export
print.check_df <- function(x, ...) {
  txt <- cd_txt(attr(x, "language") %||% "en")

  # Use cli's cat_* family so the output goes to stdout (and is therefore not
  # suppressed by `message = FALSE`, e.g. in a knitr chunk).
  cli::cat_rule(sprintf(txt$heading, nrow(x$overview), attr(x, "n_row")))
  print(x$overview)
  cli::cat_line()

  cli::cat_rule(sprintf(txt$hints_heading, nrow(x$issues)))
  if (nrow(x$issues) == 0L) {
    cli::cat_bullet(txt$no_issues, bullet = "tick", bullet_col = "green")
  } else {
    cli::cat_bullet(paste0(x$issues$variable, ": ", x$issues$hint),
      bullet = "warning", bullet_col = "yellow")
  }
  invisible(x)
}

