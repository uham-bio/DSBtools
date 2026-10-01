# Internal helpers for check_df(): pitfall detectors and translations.
# Not exported. All strings are ASCII; non-ASCII uses \uXXXX escapes.

# ---------------------------------------------------------------------------
# Pitfall detectors (language-independent, return data not text)
# ---------------------------------------------------------------------------

# Numeric, whole-numbered, and few distinct values -> may be category codes.
cd_is_codes <- function(v, threshold = 10) {
  is.numeric(v) && all_whole(v) &&
    n_distinct(v) >= 2 && n_distinct(v) <= threshold
}

# Stored as double, all whole numbers, many distinct values -> may be counts.
cd_is_count_double <- function(v, threshold = 10) {
  is.double(v) && all_whole(v) && n_distinct(v) > threshold
}

# Character / unordered factor whose non-missing values all parse as numbers
# -> numbers stored as text.
cd_num_as_text <- function(v) {
  if (!(is.character(v) || (is.factor(v) && !is.ordered(v)))) return(FALSE)
  vv <- as.character(v)
  vv <- vv[!is.na(vv) & trimws(vv) != ""]
  if (length(vv) == 0) return(FALSE)
  nums <- suppressWarnings(as.numeric(vv))
  all(!is.na(nums))
}

# Groups of raw category labels that collapse to one value after trimming
# whitespace and lower-casing -> possible spelling variants of one category.
# Returns a list of character vectors (one per affected category); empty if none.
cd_spelling_variants <- function(v) {
  if (!(is.character(v) || is.factor(v))) return(list())
  raw <- as.character(v)
  raw <- raw[!is.na(raw)]
  if (length(raw) == 0) return(list())
  key   <- tolower(trimws(raw))
  pairs <- unique(data.frame(raw = raw, key = key, stringsAsFactors = FALSE))
  by_key <- split(pairs$raw, pairs$key)
  variants <- by_key[vapply(by_key, function(x) length(unique(x)) > 1, logical(1))]
  lapply(unname(variants), unique)
}

# Count of values outside the 1.5 x IQR (Tukey) fences, for numeric vectors.
# Returns a list(n, values, lo, hi); n = 0 when the check does not apply.
cd_outliers <- function(v) {
  if (!is.numeric(v)) return(list(n = 0L, values = numeric(0), lo = NA, hi = NA))
  v <- v[!is.na(v)]
  if (length(v) < 4 || n_distinct(v) < 5)
    return(list(n = 0L, values = numeric(0), lo = NA, hi = NA))
  q   <- quantile(v, c(0.25, 0.75), names = FALSE)
  iqr <- q[2] - q[1]
  if (iqr == 0) return(list(n = 0L, values = numeric(0), lo = NA, hi = NA))
  lo  <- q[1] - 1.5 * iqr
  hi  <- q[2] + 1.5 * iqr
  out <- v[v < lo | v > hi]
  list(n = length(out), values = out, lo = lo, hi = hi)
}

# Detect all issues for one column. Returns a list of issues, each a list with
# a fixed English `issue` code and a translated `hint` (with a concrete
# suggestion). Empty list if nothing is flagged.
cd_column_issues <- function(v, txt, code_threshold = 10) {
  out <- list()
  add <- function(code, hint) out[[length(out) + 1L]] <<- list(issue = code, hint = hint)

  if (cd_is_codes(v, code_threshold))
    add("category_codes", sprintf(txt$hint_codes, n_distinct(v)))
  if (cd_is_count_double(v, code_threshold))
    add("counts_as_double", txt$hint_count)
  if (cd_num_as_text(v))
    add("numbers_as_text", txt$hint_text)
  variants <- cd_spelling_variants(v)
  if (length(variants) > 0) {
    grp   <- vapply(variants, function(g) paste0("'", g, "'", collapse = " / "),
      character(1))
    shown <- grp[seq_len(min(3L, length(grp)))]
    extra <- if (length(grp) > 3L) sprintf(" (+%d)", length(grp) - 3L) else ""
    add("spelling_variants",
      sprintf(txt$hint_variants, paste0(paste(shown, collapse = "; "), extra)))
  }
  ol <- cd_outliers(v)
  if (ol$n > 0)
    add("outliers",
      sprintf(txt$hint_outliers, ol$n, fmt_num(ol$lo, 3), fmt_num(ol$hi, 3)))
  out
}

# ---------------------------------------------------------------------------
# Translations (only hints and print headings are translated)
# ---------------------------------------------------------------------------

cd_txt <- function(language) {
  if (language == "de") list(
    heading       = "Datencheck: %d Variablen, %d Zeilen",
    hints_heading = "Hinweise (%d)",
    no_issues     = "Keine typischen Probleme gefunden.",
    hint_codes    = paste0(
      "als Zahl gespeichert, aber nur %d verschiedene ganze Werte; vielleicht ",
      "Codes f\u00fcr Kategorien (z. B. 1 = Kontrolle, 2 = Behandlung)? Wandle sie mit factor() um."),
    hint_count    = paste0(
      "als double gespeichert, aber alle Werte sind ganze Zahlen; falls es ",
      "Z\u00e4hlwerte sind, wandle sie mit as.integer() um."),
    hint_text     = paste0(
      "sieht numerisch aus, ist aber als Text gespeichert; wandle sie mit ",
      "as.numeric() um (pr\u00fcfe das Dezimaltrennzeichen)."),
    hint_variants = paste0(
      "m\u00f6gliche Schreibvarianten derselben Kategorie: %s; bereinige sie mit ",
      "trimws()/tolower() oder kodiere sie um."),
    hint_outliers = paste0(
      "%d Wert(e) au\u00dferhalb des 1.5xIQR-Bereichs [%s, %s]; pr\u00fcfe sie mit ",
      "explore_var() oder einem Boxplot.")
  ) else list(
    heading       = "Data check: %d variables, %d rows",
    hints_heading = "Hints (%d)",
    no_issues     = "No typical issues found.",
    hint_codes    = paste0(
      "stored as a number but only %d distinct whole values; maybe codes for ",
      "categories (e.g. 1 = control, 2 = treatment)? Convert with factor()."),
    hint_count    = paste0(
      "stored as double but all values are whole numbers; if these are counts, ",
      "convert with as.integer()."),
    hint_text     = paste0(
      "looks numeric but is stored as text; convert with as.numeric() (check ",
      "the decimal separator)."),
    hint_variants = paste0(
      "possible spelling variants of one category: %s; clean with ",
      "trimws()/tolower() or recode."),
    hint_outliers = paste0(
      "%d value(s) outside the 1.5xIQR range [%s, %s]; inspect with ",
      "explore_var() or a boxplot.")
  )
}
