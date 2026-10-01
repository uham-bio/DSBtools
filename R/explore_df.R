#' Explore every column of a data frame
#'
#' @description
#' `explore_df()` applies [explore_var()] to every column of a data frame.
#' Without a target variable, it produces one figure per column; with a target
#' variable, it plots every other column against the target, which allows a
#' quick screening of potential relationships.
#'
#' @param data A data frame.
#' @param target A single character string with the name of a column of `data`
#'   that every other column is plotted against, or `NULL` (default) for one
#'   figure per column.
#' @param max_bars A single positive integer giving the maximum number of bars
#'   for a discrete variable. If the number of possible values between minimum
#'   and maximum (`max - min + 1`) does not exceed `max_bars`, a bar chart with
#'   one bar per value is drawn; otherwise, the values are grouped into classes
#'   and shown as a histogram. Defaults to `getOption("DSBtools.max_bars", 30)`.
#' @param bins A single positive integer giving the number of histogram bins
#'   for continuous variables, or `NULL` (default) to use Sturges' rule
#'   (`max(8, ceiling(1 + log2(N)))`).
#' @param color A single character string with a valid R color for the plots.
#'   Defaults to `"#2E86AB"`.
#' @param alpha A single number between 0 and 1 giving the fill transparency.
#'   Defaults to `0.75`.
#' @param rot_thresh A single positive integer giving the number of categories
#'   above which x-axis labels are rotated by 45 degrees. Defaults to `5`.
#' @param sig_digits A single positive integer giving the number of significant
#'   digits in the statistics tables. Defaults to `3`.
#' @param language A single character string, either `"en"` or `"de"`. Controls
#'   the language of all plots and console messages. Defaults to
#'   `getOption("DSBtools.language", "en")`.
#'
#' @details
#' Each column is passed to [explore_var()], and the figure is printed exactly
#' as if [explore_var()] had been called directly; the column names are used as
#' axis labels. If `target` is given, each column is combined with the target
#' variable, and the display depends on the combination of both types (see
#' [explore_var()]). For a categorical and a numeric variable, for instance, the
#' categorical variable is always shown on the x-axis, regardless of which of
#' the two is the target.
#'
#' Columns are processed independently: if a column cannot be plotted (e.g., a
#' constant column or one with only missing values), a message is shown and the
#' remaining columns are still processed.
#'
#' To set the type of single columns, encode them correctly in `data` before
#' calling `explore_df()` (e.g., with [base::factor()]) or call [explore_var()]
#' for these columns separately.
#'
#' @return
#' The function returns a named list of the [explore_var()] plot objects
#' invisibly, with one element per column (excluding `target`). Elements of
#' columns that could not be plotted are `NULL`. The figures are printed as a
#' side effect.
#'
#' @seealso [check_df()] for checking a data frame before exploring it, and
#'   [describe_df()] for the statistics of all columns as data.
#' @family data exploration functions
#'
#' @export
#'
#' @examples
#' # one figure per column
#' explore_df(iris)
#'
#' # every variable against a target variable
#' explore_df(iris, target = "Species")
#'
#' # shared plot arguments (here: color) are applied to every column
#' explore_df(airquality[, c("Ozone", "Temp", "Wind")], color = "darkgreen")
#'
#' # encode columns correctly before exploring them
#' aq <- airquality
#' aq$Month <- factor(aq$Month, labels = month.abb[5:9], ordered = TRUE)
#' explore_df(aq[, c("Ozone", "Month")], target = "Month")
explore_df <- function(data, target = NULL,
  max_bars   = getOption("DSBtools.max_bars", 30),
  bins       = NULL,
  color      = "#2E86AB",
  alpha      = 0.75,
  rot_thresh = 5,
  sig_digits = 3,
  language   = getOption("DSBtools.language", "en")) {

  language <- match.arg(language, c("en", "de"))
  if (!is.data.frame(data))
    stop("`data` must be a data frame.", call. = FALSE)
  if (ncol(data) == 0L)
    stop("`data` has no columns.", call. = FALSE)
  if (!is.null(target)) {
    if (!is.character(target) || length(target) != 1L || is.na(target))
      stop("`target` must be a single column name.", call. = FALSE)
    if (!target %in% names(data))
      stop(sprintf("`target` column '%s' was not found in `data`.", target),
        call. = FALSE)
  }

  txt    <- edf_txt(language)
  tlabel <- ev_txt(language)
  cols   <- setdiff(names(data), target)
  if (length(cols) == 0L)
    stop("There are no variables left to explore.", call. = FALSE)

  message(sprintf(txt$intro, length(cols),
    if (is.null(target)) txt$no_target else sprintf(txt$vs_target, target)))

  # explore_df() prints its own compact per-column summary, so explore_var()'s
  # own type message is suppressed for the duration of the loop. Errors still
  # propagate (we gate only message(), not stop()), so failed columns are caught
  # and reported below.
  .ds_env$explore_quiet <- TRUE
  on.exit(.ds_env$explore_quiet <- FALSE, add = TRUE)

  plots <- stats::setNames(vector("list", length(cols)), cols)
  for (nm in cols) {
    v   <- data[[nm]]
    typ <- detect_type(v)
    message(sprintf("- %s: %s", nm, type_label(typ, tlabel)))

    # hint only for the ambiguous cases, indented and marked with "!"
    if (is.integer(v) && n_distinct(v) <= 10)
      message(sprintf("  ! %s", sprintf(txt$hint_codes, nm, n_distinct(v))))
    else if (is.double(v) && all_whole(v))
      message(sprintf("  ! %s", sprintf(txt$hint_whole, nm)))

    res <- tryCatch(
      if (is.null(target))
        explore_var(v, xlab = nm,
          max_bars = max_bars, bins = bins, color = color, alpha = alpha,
          rot_thresh = rot_thresh, sig_digits = sig_digits, language = language)
      else
        explore_var(v, data[[target]], xlab = nm, ylab = target,
          max_bars = max_bars, bins = bins, color = color, alpha = alpha,
          rot_thresh = rot_thresh, sig_digits = sig_digits, language = language),
      error = function(e) {
        message(sprintf("  ! %s", sprintf(txt$failed, nm, conditionMessage(e))))
        NULL
      }
    )
    # single-bracket assignment keeps NULL entries (a failed column stays in the
    # list as NULL); `plots[[nm]] <- NULL` would drop the element instead.
    plots[nm] <- list(res)
  }

  invisible(plots)
}

# Translations for explore_df(). The [explore_df] prefix appears only on the
# intro line; per-column output is a plain bullet, and hints are indented. The
# type labels themselves come from ev_txt() via type_label().
edf_txt <- function(language) {
  if (language == "de") list(
    intro      = "[explore_df] %d Variable(n) werden dargestellt (%s):",
    no_target  = "einzeln",
    vs_target  = "jeweils gegen '%s'",
    hint_codes = paste0("'%s' ist als integer gespeichert und hat nur %d ",
      "verschiedene Werte. Falls es Codes f\u00fcr Kategorien sind, wandle sie ",
      "mit factor() um oder kodiere die Spalte vor dem Aufruf von explore_df()."),
    hint_whole = paste0("'%s' enth\u00e4lt nur ganze Zahlen, ist aber als double ",
      "gespeichert und wird daher als kontinuierlich behandelt. Falls es ",
      "Z\u00e4hlwerte sind, wandle sie mit as.integer() um oder kodiere die ",
      "Spalte vor dem Aufruf von explore_df()."),
    failed     = "'%s' konnte nicht dargestellt werden: %s"
  ) else list(
    intro      = "[explore_df] Plotting %d variable(s) (%s):",
    no_target  = "one by one",
    vs_target  = "each against '%s'",
    hint_codes = paste0("'%s' is stored as integer and has only %d distinct ",
      "values. If these are codes for categories, convert with factor() or ",
      "encode the column before calling explore_df()."),
    hint_whole = paste0("'%s' contains only whole numbers but is stored as ",
      "double and is therefore treated as continuous. If these are counts, ",
      "convert with as.integer() or encode the column before calling ",
      "explore_df()."),
    failed     = "'%s' could not be plotted: %s"
  )
}
