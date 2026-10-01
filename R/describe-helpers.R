# Internal engine shared by describe_var(), describe_df() and explore_var().
# Not exported. Column names are fixed English snake_case. All strings ASCII.

# ---------------------------------------------------------------------------
# Per-variable summaries (single source of truth for the statistics)
# ---------------------------------------------------------------------------

# Full numeric summary of a vector (NA removed; missing count kept separately).
# The 95% CI always uses the t-distribution (via mean_ci()).
ds_num_summary <- function(v) {
  v         <- as.numeric(v)
  n_missing <- sum(is.na(v))
  vc        <- v[!is.na(v)]
  ci        <- mean_ci(vc)
  list(n = ci$n, n_missing = n_missing,
    mean = ci$mean, median = median(vc), sd = sd(vc), var = var(vc),
    iqr = IQR(vc), min = min(vc), max = max(vc),
    skewness = ds_skewness(vc), kurtosis = ds_kurtosis(vc),
    se = ci$se, ci_lower = ci$lo, ci_upper = ci$hi)
}

# Frequency tibble (category, n, rel) for a categorical vector; `rel` is a
# proportion in [0, 1]. Ordered factors keep their level order, other
# categoricals are ordered by descending count.
ds_freq_tbl <- function(v) {
  v <- v[!is.na(v)]
  f <- table(v)
  if (!is.ordered(v)) f <- sort(f, decreasing = TRUE)
  tibble::tibble(
    category = names(f),
    n        = as.integer(unname(f)),
    rel      = as.numeric(unname(f)) / sum(f))
}

# ---------------------------------------------------------------------------
# Table builders (one variable, optionally grouped)
# ---------------------------------------------------------------------------

# One numeric-summary row (no grouping columns).
ds_num_row <- function(v, variable) {
  s <- ds_num_summary(v)
  data.frame(variable = variable, n = s$n, n_missing = s$n_missing,
    mean = s$mean, median = s$median, sd = s$sd, var = s$var, iqr = s$iqr,
    min = s$min, max = s$max, skewness = s$skewness, kurtosis = s$kurtosis,
    se = s$se, ci_lower = s$ci_lower, ci_upper = s$ci_upper,
    stringsAsFactors = FALSE)
}

# One summary row for a categorical variable (no grouping columns):
# variable, type, n, n_missing, n_categories, mode, mode_rel.
ds_cat_summary_row <- function(v, variable, type) {
  n_missing <- sum(is.na(v))
  vc        <- v[!is.na(v)]
  n         <- length(vc)
  nd        <- n_distinct(v)
  if (n == 0L) {
    mode_val <- NA_character_
    mode_rel <- NA_real_
  } else {
    f        <- table(vc)
    i        <- which.max(f)
    mode_val <- names(f)[i]
    mode_rel <- as.numeric(f[i]) / n
  }
  data.frame(variable = variable, type = type, n = n, n_missing = n_missing,
    n_categories = nd, mode = mode_val, mode_rel = mode_rel,
    stringsAsFactors = FALSE)
}

# Long frequency rows for a categorical variable (no grouping columns):
# variable, category, n, rel. Nominal categories are ordered by descending
# frequency and, when there are more than `max_categories`, the least frequent
# ones are lumped into a single "(other)" row (n and rel still sum correctly).
# Ordinal variables keep their level order and are never lumped.
ds_cat_freq_rows <- function(v, variable, type, max_categories, other_label) {
  f <- ds_freq_tbl(v)
  if (identical(type, "nominal") && is.finite(max_categories) &&
      nrow(f) > max_categories) {
    keep  <- max(as.integer(max_categories) - 1L, 0L)
    top   <- f[seq_len(keep), , drop = FALSE]
    rest  <- f[-seq_len(keep), , drop = FALSE]
    other <- data.frame(category = other_label, n = sum(rest$n),
      rel = sum(rest$rel), stringsAsFactors = FALSE)
    f <- rbind(as.data.frame(top), other)
  }
  data.frame(variable = variable, category = f$category, n = f$n, rel = f$rel,
    stringsAsFactors = FALSE)
}

# Apply `fun` (returning a data.frame) to `x` within each group defined by the
# columns of `grp_df`, prefixing each result with the grouping columns. Rows
# with a missing grouping value are dropped; groups are ordered by the grouping
# variables (factor level order).
ds_group_apply <- function(x, grp_df, fun) {
  keep   <- stats::complete.cases(grp_df)
  x      <- x[keep]
  grp_df <- grp_df[keep, , drop = FALSE]
  if (nrow(grp_df) == 0L) return(fun(x[0]))

  ord    <- do.call(order, unname(as.list(grp_df)))
  x      <- x[ord]
  grp_df <- grp_df[ord, , drop = FALSE]
  key    <- do.call(paste, c(unname(as.list(grp_df)), sep = "\r"))
  starts <- which(!duplicated(key))
  ends   <- c(starts[-1] - 1L, length(key))

  parts <- lapply(seq_along(starts), function(i) {
    res   <- fun(x[starts[i]:ends[i]])
    gcols <- grp_df[rep(starts[i], nrow(res)), , drop = FALSE]
    cbind(gcols, res, row.names = NULL)
  })
  do.call(rbind, parts)
}

# Numeric table for one variable (one row per group combination, or one row).
ds_build_numeric <- function(x, variable, grp_df) {
  if (is.null(grp_df))
    return(tibble::as_tibble(ds_num_row(x, variable)))
  tibble::as_tibble(ds_group_apply(x, grp_df, function(v) ds_num_row(v, variable)))
}

# Categorical table for one variable. With `frequencies = FALSE` it is one
# summary row (per group); with `frequencies = TRUE` it is the long frequency
# table (`rel` within each group).
ds_build_categorical <- function(x, variable, type, grp_df, frequencies,
  max_categories, other_label) {
  fun <- if (frequencies)
    function(v) ds_cat_freq_rows(v, variable, type, max_categories, other_label)
  else
    function(v) ds_cat_summary_row(v, variable, type)
  if (is.null(grp_df)) return(tibble::as_tibble(fun(x)))
  tibble::as_tibble(ds_group_apply(x, grp_df, fun))
}

# ---------------------------------------------------------------------------
# Empty (0-row) templates, carrying the grouping columns when grouped
# ---------------------------------------------------------------------------

ds_empty_numeric <- function(grp_df) {
  base <- tibble::tibble(
    variable = character(0), n = integer(0), n_missing = integer(0),
    mean = numeric(0), median = numeric(0), sd = numeric(0), var = numeric(0),
    iqr = numeric(0), min = numeric(0), max = numeric(0),
    skewness = numeric(0), kurtosis = numeric(0), se = numeric(0),
    ci_lower = numeric(0), ci_upper = numeric(0))
  if (is.null(grp_df)) return(base)
  tibble::as_tibble(cbind(grp_df[0, , drop = FALSE], base))
}

ds_empty_categorical <- function(grp_df, frequencies) {
  base <- if (frequencies)
    tibble::tibble(variable = character(0), category = character(0),
      n = integer(0), rel = numeric(0))
  else
    tibble::tibble(variable = character(0), type = character(0),
      n = integer(0), n_missing = integer(0), n_categories = integer(0),
      mode = character(0), mode_rel = numeric(0))
  if (is.null(grp_df)) return(base)
  tibble::as_tibble(cbind(grp_df[0, , drop = FALSE], base))
}

# ---------------------------------------------------------------------------
# Grouping helpers
# ---------------------------------------------------------------------------

# Keep only the part of a name after the last "$" (e.g. iris$Species -> Species).
strip_dollar <- function(x) sub("^.*\\$", "", x)

# Normalise the `by` argument of describe_var() to a named list of grouping
# vectors, or NULL. `by_expr` is substitute(by) from the caller.
normalize_by <- function(by, by_expr) {
  if (is.null(by)) return(NULL)
  if (is.data.frame(by)) return(as.list(by))
  if (is.list(by)) {
    nm <- names(by)
    if (is.null(nm) || any(!nzchar(nm)))
      stop("all elements of `by` must be named.", call. = FALSE)
    return(by)
  }
  stats::setNames(list(by), strip_dollar(deparse1(by_expr)))
}

# Validate a named list of grouping vectors and return them as a data.frame.
ds_grp_df <- function(by_list, n) {
  for (nm in names(by_list)) {
    g <- by_list[[nm]]
    if (length(g) != n)
      stop(sprintf("grouping variable '%s' must have the same length as `x`.",
        nm), call. = FALSE)
    typ <- detect_type(g)
    if (!typ %in% c("nominal", "ordinal"))
      stop(sprintf(
        "grouping variable '%s' must be categorical (nominal or ordinal), not %s.",
        nm, typ), call. = FALSE)
  }
  as.data.frame(by_list, stringsAsFactors = FALSE, check.names = FALSE)
}

# ---------------------------------------------------------------------------
# Result object + translations
# ---------------------------------------------------------------------------

new_ds_description <- function(x, language = "en")
  structure(x, class = "ds_description", language = language)

# Headings, lump label, and console hints. All strings ASCII (\uXXXX escapes).
dv_txt <- function(language) {
  if (language == "de")
    list(
      num        = "Numerische Variablen",
      cat        = "Kategoriale Variablen",
      other      = "(sonstige)",
      hint_cols  = paste0("Alle Spalten anzeigen mit View(<Ergebnis>%s) oder ",
        "print(<Ergebnis>%s, width = Inf)."),
      hint_id    = "'%s' sieht wie ein Bezeichner aus (%d verschiedene Werte in %d Zeilen).",
      hint_codes = paste0("'%s' ist als integer mit nur %d verschiedenen Werten ",
        "gespeichert; das k\u00f6nnten Codes f\u00fcr Kategorien sein -- wandle sie mit factor() um."),
      hint_whole = paste0("'%s' ist als double gespeichert, aber alle Werte sind ",
        "ganze Zahlen; das k\u00f6nnten Z\u00e4hlwerte sein -- wandle sie mit as.integer() um."))
  else
    list(
      num        = "Numeric variables",
      cat        = "Categorical variables",
      other      = "(other)",
      hint_cols  = paste0("Show all columns with View(<result>%s) or ",
        "print(<result>%s, width = Inf)."),
      hint_id    = "'%s' looks like an identifier (%d distinct values in %d rows).",
      hint_codes = paste0("'%s' is stored as integer with only %d distinct values; ",
        "these might be category codes -- consider factor()."),
      hint_whole = paste0("'%s' is stored as double but all values are whole ",
        "numbers; these might be counts -- consider as.integer()."))
}

# Emit console hints for one variable: an identifier hint for high-cardinality
# categoricals, and the same type hints as explore_var() for numeric columns
# (integer with few distinct values -> codes; whole-number double -> counts).
# `type_hints = FALSE` suppresses the numeric type hints (e.g. when the type was
# set explicitly).
ds_emit_hints <- function(v, variable, type, language, prefix, type_hints = TRUE) {
  txt <- dv_txt(language)
  emit <- function(msg) message(sprintf("%s %s", prefix, msg))
  if (type %in% c("nominal", "ordinal")) {
    nn <- sum(!is.na(v))
    nd <- n_distinct(v)
    if (nn > 0L && nd >= 0.5 * nn)
      emit(sprintf(txt$hint_id, variable, nd, nn))
  } else if (isTRUE(type_hints)) {
    if (is.integer(v) && n_distinct(v) <= 10)
      emit(sprintf(txt$hint_codes, variable, n_distinct(v)))
    else if (is.double(v) && all_whole(v))
      emit(sprintf(txt$hint_whole, variable))
  }
  invisible(NULL)
}

# TRUE if, at the given console width, pillar would hide some columns of `tbl`
# (i.e. the printed tibble shows fewer columns than ncol(tbl)).
ds_cols_hidden <- function(tbl, width = getOption("width")) {
  setup <- pillar::tbl_format_setup(tbl, width = width)
  length(setup$extra_cols) > 0L
}

# Print (to stdout) a translated hint on how to see all columns, but only when
# some columns are hidden at the current console width. `comp` is the list
# component the table lives in ("$numeric" or "$categorical").
ds_emit_cols_hint <- function(tbl, comp, txt) {
  if (ds_cols_hidden(tbl))
    cli::cat_line(cli::col_grey(sprintf(txt$hint_cols, comp, comp)))
  invisible(NULL)
}
