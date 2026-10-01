# Internal helper functions for explore_var().
#
# None of these are exported. Package-wide imports (ggplot2, stats, ...) are
# declared in the roxygen block of explore_var() in R/explore_var.R; because
# `importFrom` applies to the whole package namespace, the functions below can
# use the imported functions (sd, qt, median, ...) directly.
#
# explore_var() itself only resolves the variable types and dispatches to one of
# the five plot builders below (ev_plot_cat(), ev_plot_num(), ev_plot_num_num(),
# ev_plot_cat_num(), ev_plot_cat_cat()). Those builders, and the small shared
# drawing helpers, take a `cfg` list with the graphical arguments (color, alpha,
# bins, max_bars, rot_thresh, sig_digits), the translations `txt`, and a bound
# number formatter `fmt`. The remaining helpers can be tested in isolation: type
# detection/conversion, translations, and statistics.
#
# All strings are ASCII; non-ASCII characters use \uXXXX escapes.

# ---------------------------------------------------------------------------
# Translations
# ---------------------------------------------------------------------------

# Return the list of language-specific strings used throughout explore_var().
# `language` must already be one of "en" or "de" (validated by the caller).
# Strings used as sprintf() formats contain %s / %d; literal percent signs in
# those strings are escaped as %%.
ev_txt <- function(language) {
  if (language == "de") list(
    type_nominal  = "nominal",        type_ordinal  = "ordinal",
    type_discrete = "diskret",        type_cont     = "kontinuierlich",
    lbl_type      = "Typ",            lbl_n         = "N",
    lbl_missing   = "fehlend",        lbl_miss_pair = "fehlend (paarweise)",
    col_statistic = "Kennzahl",
    stat_n        = "N",              stat_mean     = "Mittelwert",
    stat_ci       = "95%-KI",         stat_var      = "Varianz",
    stat_sd       = "SD",             stat_se       = "SE (MW)",
    stat_median   = "Median",         stat_iqr      = "IQR",
    stat_range    = "Min / Max",
    stat_skew     = "Schiefe",        stat_kurt     = "W\u00f6lbung (Kurtosis)",
    col_group     = "Gruppe",         col_ci        = "95%-KI",
    col_range     = "Min / Max",
    col_category  = "Kategorie",      col_n_abs     = "N (abs.)",
    col_n_rel     = "N (rel.)",
    lbl_count     = "H\u00e4ufigkeit (N)",
    lbl_freq_bare = "H\u00e4ufigkeit",
    title_freqdist  = "H\u00e4ufigkeitsverteilung: %s",
    title_dist      = "Verteilung: %s",
    title_scatter   = "Streudiagramm: %s vs. %s",
    title_by        = "%s nach %s",
    title_violin    = "Violinplot",
    title_ci        = "Mittelwert \u00b1 95%-KI",
    title_freqcross = "H\u00e4ufigkeiten: %s \u00d7 %s",
    sub_grand_mean  = "Gestrichelt = Gesamtmittelwert",
    cap_scatter     = "Rot = lin. Regression (95%-KI)  |  Gr\u00fcn gestrichelt = LOESS",
    cap_boxplot     = "Roter Punkt + Balken = Mittelwert \u00b1 95%-KI",
    cap_chisq       = "Chi-Quadrat-Unabh\u00e4ngigkeitstest: Chi-Quadrat = %s, df = %d, p %s",
    cap_chisq_warn  = "  (erwartete H\u00e4ufigkeiten < 5: mit Vorsicht interpretieren)",
    lbl_mean_short  = "MW",           lbl_median_short = "Md",
    src_detected    = "automatisch erkannt",
    src_specified   = "festgelegt",
    msg_type        = "[explore_var] Typ von '%s': %s (%s)",
    msg_codes       = paste0(
      "[explore_var] '%s' ist als integer gespeichert und hat nur %d ",
      "verschiedene Werte. Falls es Codes f\u00fcr Kategorien sind ",
      "(z. B. 1 = Kontrolle, 2 = Behandlung), wandle sie mit factor() um oder ",
      "setze %s = \"nominal\" (bzw. \"ordinal\")."),
    msg_whole       = paste0(
      "[explore_var] '%s' enth\u00e4lt nur ganze Zahlen, ist aber als double ",
      "gespeichert und wird daher als kontinuierlich behandelt. Falls es ",
      "Z\u00e4hlwerte sind, setze %s = \"discrete\" oder wandle sie mit ",
      "as.integer() um."),
    msg_ord_levels  = paste0(
      "[explore_var] Die Stufen von '%s' wurden alphabetisch sortiert: %s. ",
      "Lege eine andere Reihenfolge mit factor(..., levels = ..., ordered = TRUE) ",
      "fest."),
    err_not_numeric = "'%s' kann nicht als %s behandelt werden: Die Werte sind nicht numerisch.",
    err_not_whole   = "'%s' kann nicht als diskret behandelt werden: Nicht alle Werte sind ganze Zahlen."
  ) else list(
    type_nominal  = "nominal",        type_ordinal  = "ordinal",
    type_discrete = "discrete",       type_cont     = "continuous",
    lbl_type      = "Type",           lbl_n         = "N",
    lbl_missing   = "missing",        lbl_miss_pair = "missing (pairwise)",
    col_statistic = "Statistic",
    stat_n        = "N",              stat_mean     = "Mean",
    stat_ci       = "95% CI",         stat_var      = "Variance",
    stat_sd       = "SD",             stat_se       = "SE (mean)",
    stat_median   = "Median",         stat_iqr      = "IQR",
    stat_range    = "Min / Max",
    stat_skew     = "Skewness",       stat_kurt     = "Kurtosis",
    col_group     = "Group",          col_ci        = "95% CI",
    col_range     = "Min / Max",
    col_category  = "Category",       col_n_abs     = "N (abs.)",
    col_n_rel     = "N (rel.)",
    lbl_count     = "Frequency (N)",
    lbl_freq_bare = "Frequency",
    title_freqdist  = "Frequency distribution: %s",
    title_dist      = "Distribution: %s",
    title_scatter   = "Scatter plot: %s vs. %s",
    title_by        = "%s by %s",
    title_violin    = "Violin plot",
    title_ci        = "Mean \u00b1 95% CI",
    title_freqcross = "Frequencies: %s \u00d7 %s",
    sub_grand_mean  = "Dashed = grand mean",
    cap_scatter     = "Red = lin. regression (95% CI)  |  Green dashed = LOESS",
    cap_boxplot     = "Red dot + bar = mean \u00b1 95% CI",
    cap_chisq       = "Chi-squared test of independence: Chi-squared = %s, df = %d, p %s",
    cap_chisq_warn  = "  (expected counts < 5: interpret with caution)",
    lbl_mean_short  = "Mean",         lbl_median_short = "Median",
    src_detected    = "detected automatically",
    src_specified   = "specified",
    msg_type        = "[explore_var] Type of '%s': %s (%s)",
    msg_codes       = paste0(
      "[explore_var] '%s' is stored as integer and has only %d distinct ",
      "values. If these are codes for categories (e.g. 1 = control, ",
      "2 = treatment), convert with factor() or set %s = \"nominal\" ",
      "(or \"ordinal\")."),
    msg_whole       = paste0(
      "[explore_var] '%s' contains only whole numbers but is stored as ",
      "double and is therefore treated as continuous. If these are counts, ",
      "set %s = \"discrete\" or convert with as.integer()."),
    msg_ord_levels  = paste0(
      "[explore_var] The levels of '%s' were sorted alphabetically: %s. ",
      "Use factor(..., levels = ..., ordered = TRUE) to set a different ",
      "order."),
    err_not_numeric = "'%s' cannot be treated as %s: the values are not numeric.",
    err_not_whole   = "'%s' cannot be treated as discrete: not all values are whole numbers."
  )
}

# Localised label for a detected/specified type.
type_label <- function(typ, txt) switch(typ,
  nominal    = txt$type_nominal,  ordinal    = txt$type_ordinal,
  discrete   = txt$type_discrete, continuous = txt$type_cont,
  typ
)

# ---------------------------------------------------------------------------
# Number formatting
# ---------------------------------------------------------------------------

# Smart number formatter: whole numbers stay integers, floats use signif().
fmt_num <- function(v, sig) {
  vapply(v, function(x) {
    if (is.na(x))                        return(NA_character_)
    if (x == round(x) && abs(x) < 1e15) return(as.character(as.integer(x)))
    as.character(signif(x, sig))
  }, character(1))
}

# Format a p-value for display.
fmt_p <- function(p) {
  if (is.na(p)) return("= NA")
  if (p < 0.001) "< 0.001" else sprintf("= %.3f", p)
}

# ---------------------------------------------------------------------------
# Type detection & conversion
# ---------------------------------------------------------------------------

# Validate and normalise a user-supplied `x_type` / `y_type` argument.
# Returns NULL for NULL input, otherwise the matched canonical type.
norm_type <- function(t, argname) {
  valid_types <- c("nominal", "ordinal", "discrete", "continuous")
  if (is.null(t)) return(NULL)
  if (!is.character(t) || length(t) != 1L || is.na(t))
    stop(sprintf("`%s` must be a single character string.", argname),
      call. = FALSE)
  i  <- pmatch(tolower(t), valid_types)
  if (is.na(i))
    stop(sprintf("`%s` must be one of %s.", argname,
      paste0("\"", valid_types, "\"", collapse = ", ")),
      call. = FALSE)
  valid_types[i]
}

# Number of distinct non-missing values.
n_distinct <- function(v) length(unique(v[!is.na(v)]))

# TRUE if all non-missing values are whole numbers (and at least one exists).
all_whole <- function(v) {
  v <- v[!is.na(v)]
  length(v) > 0 && all(v == round(v))
}

# Detect the scale of measurement from how the variable is stored in R.
detect_type <- function(v) {
  if (is.logical(v) || is.character(v)) return("nominal")
  if (is.factor(v)) return(if (is.ordered(v)) "ordinal" else "nominal")
  if (is.integer(v)) return("discrete")
  "continuous"
}

# Convert a vector so that it matches a user-specified type.
apply_type <- function(v, typ, name, txt) {
  if (typ == "nominal") {
    return(if (is.factor(v)) factor(v, levels = levels(v), ordered = FALSE)
      else factor(v))
  }
  if (typ == "ordinal") {
    if (is.factor(v)) return(factor(v, levels = levels(v), ordered = TRUE))
    f <- factor(v, ordered = TRUE)
    if (!is.numeric(v))
      message(sprintf(txt$msg_ord_levels, name,
        paste(levels(f), collapse = " < ")))
    return(f)
  }
  # discrete / continuous
  num <- suppressWarnings(
    if (is.factor(v)) as.numeric(as.character(v)) else as.numeric(v))
  if (sum(is.na(num)) > sum(is.na(v)))
    stop(sprintf(txt$err_not_numeric, name, type_label(typ, txt)), call. = FALSE)
  if (typ == "discrete" && any(num != round(num), na.rm = TRUE))
    stop(sprintf(txt$err_not_whole, name), call. = FALSE)
  num
}

# Detect (and message) or apply the type of a variable. Returns a list with the
# (possibly converted) vector `v` and the resolved `type`. The console messages
# are suppressed while explore_df() is running (it prints its own compact
# summary); only message() is gated, so errors from apply_type() still propagate.
resolve_var <- function(v, user_type, name, argname, txt) {
  quiet <- isTRUE(.ds_env$explore_quiet)
  if (is.null(user_type)) {
    typ <- detect_type(v)
    if (!quiet) {
      message(sprintf(txt$msg_type, name, type_label(typ, txt), txt$src_detected))
      if (typ == "discrete" && n_distinct(v) <= 10)
        message(sprintf(txt$msg_codes, name, n_distinct(v), argname))
      if (typ == "continuous" && is.double(v) && all_whole(v))
        message(sprintf(txt$msg_whole, name, argname))
    }
    list(v = v, type = typ)
  } else {
    if (!quiet)
      message(sprintf(txt$msg_type, name, type_label(user_type, txt),
        txt$src_specified))
    list(v = apply_type(v, user_type, name, txt), type = user_type)
  }
}

# ---------------------------------------------------------------------------
# Statistics
# ---------------------------------------------------------------------------

# Mean and two-sided 95% confidence interval. The critical value is always taken
# from the t-distribution with df = n - 1.
mean_ci <- function(v) {
  n  <- length(v)
  se <- sd(v) / sqrt(n)
  tcrit <- qt(0.975, df = n - 1)
  list(n = n, mean = mean(v), se = se,
    lo = mean(v) - tcrit * se, hi = mean(v) + tcrit * se)
}

# -- Display formatters -------------------------------------------------------
# These turn the raw numeric tibbles produced by describe_var() (single source
# of truth) into localised, rounded data.frames for the figure tables. They do
# not compute anything statistical themselves.

# Vertical statistics table for one numeric variable, from a one-row stats
# tibble (columns n, mean, median, sd, var, iqr, min, max, skewness, kurtosis,
# se, ci_lower, ci_upper). Rows are ordered by role: sample size -- location --
# spread -- inference.
fmt_num_table <- function(row, label, txt, sig_digits) {
  fmt <- function(val) fmt_num(val, sig_digits)
  df_s <- data.frame(
    Stat = c(txt$stat_n,
      txt$stat_mean, txt$stat_median,
      txt$stat_sd, txt$stat_var, txt$stat_iqr, txt$stat_range,
      txt$stat_skew, txt$stat_kurt,
      txt$stat_se, txt$stat_ci),
    Val  = c(fmt(row$n),
      fmt(row$mean), fmt(row$median),
      fmt(row$sd), fmt(row$var), fmt(row$iqr),
      sprintf("%s / %s", fmt(row$min), fmt(row$max)),
      fmt(row$skewness), fmt(row$kurtosis),
      fmt(row$se),
      sprintf("%s \u2013 %s", fmt(row$ci_lower), fmt(row$ci_upper))),
    stringsAsFactors = FALSE
  )
  names(df_s) <- c(txt$col_statistic, label)
  df_s
}

# Wide per-group table, from a per-group stats tibble (one row per group) as
# produced by the shared engine ds_build_numeric(), i.e. with columns group, n,
# n_missing, mean, median, sd, var, iqr, min, max, skewness, kurtosis, se,
# ci_lower, ci_upper. Columns follow the same order as fmt_num_table(), with the
# group and missing-count columns in front.
fmt_group_table <- function(gtbl, txt, sig_digits) {
  fmt <- function(val) fmt_num(val, sig_digits)
  out <- data.frame(
    G    = as.character(gtbl$group),
    N    = fmt(gtbl$n),
    NMIS = fmt(gtbl$n_missing),
    MW   = fmt(gtbl$mean),
    Md   = fmt(gtbl$median),
    SD   = fmt(gtbl$sd),
    VAR  = fmt(gtbl$var),
    IQR  = fmt(gtbl$iqr),
    R    = sprintf("%s / %s", fmt(gtbl$min), fmt(gtbl$max)),
    SKEW = fmt(gtbl$skewness),
    KURT = fmt(gtbl$kurtosis),
    SE   = fmt(gtbl$se),
    CI   = sprintf("%s \u2013 %s", fmt(gtbl$ci_lower), fmt(gtbl$ci_upper)),
    stringsAsFactors = FALSE
  )
  names(out) <- c(txt$col_group, txt$stat_n, txt$lbl_missing, txt$stat_mean,
    txt$stat_median, txt$stat_sd, txt$stat_var, txt$stat_iqr, txt$col_range,
    txt$stat_skew, txt$stat_kurt, txt$stat_se, txt$col_ci)
  out
}

# Frequency table for display, from a freq tibble (category, n, rel) where rel
# is a proportion. The relative frequency is shown as a rounded percentage.
fmt_freq_table <- function(ftbl, txt) {
  out <- data.frame(
    category = ftbl$category,
    n        = ftbl$n,
    rel      = paste0(round(ftbl$rel * 100, 1), " %"),
    stringsAsFactors = FALSE
  )
  names(out) <- c(txt$col_category, txt$col_n_abs, txt$col_n_rel)
  out
}

# ---------------------------------------------------------------------------
# Plot building blocks (shared by the five plot builders below)
# ---------------------------------------------------------------------------

# TRUE for the categorical types.
ev_is_cat <- function(typ) typ %in% c("nominal", "ordinal")

# White-background annotation applied to every composite figure.
ev_ann_theme <- function()
  plot_annotation(
    theme = theme(plot.background = element_rect(fill = "white", color = NA)))

# A statistics/contingency table rendered as a patchwork element.
ev_stat_panel <- function(df_stats) ds_tablegrob(df_stats)

# Integer axis breaks for a discrete bar chart.
ev_int_breaks <- function(lims) {
  lo <- ceiling(lims[1]);  hi <- floor(lims[2])
  if (hi - lo <= 15) return(seq(lo, hi))
  b <- pretty(lims)
  b[b == round(b)]
}

# Distribution plot for a numeric variable. Discrete with a narrow range
# (max - min + 1 <= max_bars): one bar per possible value, gaps stay visible.
# Otherwise a histogram (Sturges' rule or `cfg$bins`); a density curve is added
# for continuous variables only.
ev_dist_plot <- function(v, typ, cfg, density_lw = 1) {
  p <- ggplot(data.frame(x = v), aes(x = x))
  if (typ == "discrete" && diff(range(v)) + 1 <= cfg$max_bars)
    return(p +
        geom_bar(width = 0.6, fill = cfg$color, alpha = cfg$alpha) +
        scale_x_continuous(breaks = ev_int_breaks))
  nb <- if (!is.null(cfg$bins)) cfg$bins
    else max(8L, ceiling(1 + log2(length(v))))
  bw <- diff(range(v)) / nb
  nv <- length(v)
  p <- p + geom_histogram(bins = nb, fill = cfg$color, alpha = cfg$alpha,
    color = "white", linewidth = 0.2)
  if (typ == "continuous")
    p <- p + geom_density(aes(y = after_stat(density) * nv * bw),
      color = scales::muted(cfg$color, l = 40),
      linewidth = density_lw, adjust = 1.2)
  p
}

# Subtitle for a single variable (type, N, missing).
ev_make_sub1 <- function(typ, n_valid, n_miss, txt)
  sprintf("%s: %s  |  %s = %d  |  %s: %d",
    txt$lbl_type, type_label(typ, txt),
    txt$lbl_n, n_valid, txt$lbl_missing, n_miss)

# Subtitle for two variables (N of complete pairs, pairwise missing).
ev_make_sub2 <- function(n_valid, n_miss, txt)
  sprintf("%s = %d%s", txt$lbl_n, n_valid,
    if (n_miss > 0) sprintf("  |  %s: %d", txt$lbl_miss_pair, n_miss) else "")

# ---------------------------------------------------------------------------
# Plot builders (one per variable-type combination). Each takes the resolved,
# NA-handled data plus the drawing context `cfg` and returns the composite
# patchwork object; explore_var() prints it.
# ---------------------------------------------------------------------------

# One categorical variable: frequency bar chart + frequency table.
ev_plot_cat <- function(x, xname, xtype, cfg) {
  txt     <- cfg$txt
  n_total <- length(x)
  n_miss  <- sum(is.na(x))
  sub_txt <- ev_make_sub1(xtype, n_total - n_miss, n_miss, txt)

  freq     <- ds_freq_tbl(x)
  freq_tbl <- fmt_freq_table(freq, txt)

  # order the bars to match the frequency table (ordered factors by level,
  # other categoricals by descending count)
  df    <- data.frame(x = factor(x[!is.na(x)], levels = freq$category))
  n_lvl <- nlevels(df$x)

  p_bar <- ggplot(df, aes(x = x)) +
    geom_bar(fill = cfg$color, alpha = cfg$alpha,
      color = "white", linewidth = 0.3) +
    geom_text(stat = "count", aes(label = after_stat(count)),
      vjust = -0.4, size = 3.5, color = "grey35") +
    labs(x = xname, y = txt$lbl_count,
      title    = sprintf(txt$title_freqdist, xname),
      subtitle = sub_txt) +
    ds_theme_ex() +
    if (n_lvl > cfg$rot_thresh) ds_theme_rot() else NULL

  (p_bar | ev_stat_panel(freq_tbl)) +
    plot_layout(widths = c(3, 1.2)) & ev_ann_theme()
}

# One numeric variable: distribution plot with mean/median lines and labels, a
# 1-D boxplot with jitter, and the statistics table.
ev_plot_num <- function(x, xname, xtype, cfg) {
  txt     <- cfg$txt
  fmt     <- cfg$fmt
  n_total <- length(x)
  n_miss  <- sum(is.na(x))
  sub_txt <- ev_make_sub1(xtype, n_total - n_miss, n_miss, txt)

  xv       <- as.numeric(na.omit(as.numeric(x)))
  summ     <- ds_num_summary(xv)
  df       <- data.frame(x = xv)
  mw       <- summ$mean
  med      <- summ$median
  col_dark <- scales::muted(cfg$color, l = 40)

  p_hist <- ev_dist_plot(xv, xtype, cfg) +
    geom_vline(xintercept = mw,  linetype = "dashed",
      color = "#E74C3C", linewidth = 0.8) +
    geom_vline(xintercept = med, linetype = "dotted",
      color = "#27AE60", linewidth = 0.8) +
    annotate("label",
      x = mw,  y = Inf,
      label = sprintf("%s = %s", txt$lbl_mean_short, fmt(mw)),
      vjust = 1.6, hjust = -0.05,
      fill = "#E74C3C", color = "white",
      size = 3, label.padding = unit(0.2, "lines"),
      linewidth = 0) +
    annotate("label",
      x = med, y = Inf,
      label = sprintf("%s = %s", txt$lbl_median_short, fmt(med)),
      vjust = 3.6, hjust = -0.05,
      fill = "#27AE60", color = "white",
      size = 3, label.padding = unit(0.2, "lines"),
      linewidth = 0) +
    labs(x = xname, y = txt$lbl_count,
      title    = sprintf(txt$title_dist, xname),
      subtitle = sub_txt) +
    ds_theme_ex()

  p_box <- ggplot(df, aes(x = x, y = 0)) +
    geom_boxplot(fill = cfg$color, alpha = cfg$alpha * 0.85,
      color         = col_dark,
      width         = 0.45,
      outlier.color = "#E74C3C",
      outlier.shape = 16,
      outlier.alpha = 0.7,
      outlier.size  = 1.8) +
    geom_jitter(aes(y = 0), height = 0.12,
      alpha = 0.25, size = 0.9, color = "grey30") +
    scale_y_continuous(limits = c(-0.4, 0.4)) +
    labs(x = xname) +
    ds_theme_ex() +
    theme(axis.title.y       = element_blank(),
      axis.text.y        = element_blank(),
      axis.ticks.y       = element_blank(),
      panel.grid.major.y = element_blank())

  stats_tbl <- fmt_num_table(summ, xname, txt, cfg$sig_digits)
  (p_hist | ev_stat_panel(stats_tbl)) / p_box +
    plot_layout(heights = c(3, 1)) & ev_ann_theme()
}

# Two numeric variables: scatter with linear regression (95% CI), equation,
# R-squared, Pearson's r, a LOESS smoother, marginal distributions, and a
# combined statistics table.
ev_plot_num_num <- function(x_cc, y_cc, xname, yname, xtype, ytype, sub2, cfg) {
  txt <- cfg$txt
  fmt <- cfg$fmt

  xv  <- as.numeric(x_cc);  yv <- as.numeric(y_cc)
  df2 <- data.frame(x = xv, y = yv)

  r  <- cor(xv, yv)
  # Linear regression line shown as geom_smooth("lm"); report its equation and
  # R^2 (= r^2 for a simple regression) next to Pearson's r.
  cf <- coef(lm(yv ~ xv))
  b0 <- unname(cf[1L]);  b1 <- unname(cf[2L])
  eq    <- sprintf("y = %s %s %s\u00b7x",
    fmt(b0), if (b1 >= 0) "+" else "-", fmt(abs(b1)))
  r_lbl <- sprintf("%s\nR\u00b2 = %s\nPearson r = %.3f  (p %s)",
    eq, fmt(r^2), r, fmt_p(cor.test(xv, yv)$p.value))

  p_scatter <- ggplot(df2, aes(x = x, y = y)) +
    geom_point(alpha = 0.45, color = cfg$color, size = 1.8) +
    geom_smooth(method = "lm",    formula = y ~ x,
      se = TRUE,  color = "#E74C3C",
      fill = "#E74C3C", alpha = 0.12, linewidth = 0.9) +
    geom_smooth(method = "loess", formula = y ~ x,
      se = FALSE, color = "#27AE60",
      linetype = "dashed", linewidth = 0.8) +
    annotate("text", x = Inf, y = -Inf, label = r_lbl,
      hjust = 1.05, vjust = -0.35, lineheight = 0.95,
      size = 3.2, fontface = "italic", color = "grey30") +
    labs(x = xname, y = yname,
      title    = sprintf(txt$title_scatter, xname, yname),
      subtitle = sub2,
      caption  = txt$cap_scatter) +
    ds_theme_ex()

  marg_hist <- function(v, typ, name) {
    ev_dist_plot(v, typ, cfg, density_lw = 0.8) +
      labs(x = name, y = txt$lbl_count,
        title = sprintf(txt$title_dist, name)) +
      ds_theme_ex(9)
  }

  p_hx <- marg_hist(xv, xtype, xname)
  p_hy <- marg_hist(yv, ytype, yname)

  sx     <- fmt_num_table(ds_num_summary(xv), xname, txt, cfg$sig_digits)
  sy     <- fmt_num_table(ds_num_summary(yv), yname, txt, cfg$sig_digits)
  s_both <- sx;  s_both[[yname]] <- sy[[2]]

  (p_scatter | ev_stat_panel(s_both)) /
    (p_hx | p_hy) +
    plot_layout(heights = c(3, 1.5)) & ev_ann_theme()
}

# One categorical and one numeric variable: grouped boxplot with the group
# mean +/- 95% CI, a violin plot, a forest plot of group means +/- 95% CI, and
# a per-group statistics table. The per-group summaries come from the same
# engine as describe_var(by = ...), so they also carry n_missing, skewness and
# kurtosis; the mean +/- CI pointrange on the boxplot is drawn from this table.
ev_plot_cat_num <- function(x_cc, y_cc, xtype, ytype, xname, yname, sub2, cfg) {
  txt <- cfg$txt

  if (ev_is_cat(xtype)) {
    cat_v <- droplevels(factor(x_cc));  num_v <- as.numeric(y_cc)
    cn <- xname;                         nn <- yname
  } else {
    cat_v <- droplevels(factor(y_cc));  num_v <- as.numeric(x_cc)
    cn <- yname;                         nn <- xname
  }

  df2   <- data.frame(g = cat_v, v = num_v)
  n_lvl <- nlevels(cat_v)

  # One summary row per group from the shared engine (variable, n, n_missing,
  # mean, ..., skewness, kurtosis, se, ci_lower, ci_upper); grouped and ordered
  # by the factor levels of `cat_v`.
  grp_tbl <- ds_build_numeric(num_v, nn, data.frame(group = cat_v))

  # Group means +/- 95% CI in the box/violin group order; reused both for the
  # boxplot pointrange and the forest plot.
  ci_data <- data.frame(
    g  = factor(as.character(grp_tbl$group), levels = levels(cat_v)),
    mw = grp_tbl$mean,
    lo = grp_tbl$ci_lower,
    hi = grp_tbl$ci_upper,
    stringsAsFactors = FALSE)

  p_bx <- ggplot(df2, aes(x = g, y = v, fill = g)) +
    geom_boxplot(alpha         = cfg$alpha,
      outlier.color = "#E74C3C",
      outlier.shape = 16,
      outlier.alpha = 0.7,
      show.legend   = FALSE) +
    geom_jitter(width = 0.12, alpha = 0.3, size = 1.2,
      color = "grey30", show.legend = FALSE) +
    geom_pointrange(data = ci_data,
      aes(x = g, y = mw, ymin = lo, ymax = hi),
      inherit.aes = FALSE, color = "#E74C3C",
      size = 0.5, linewidth = 0.7, show.legend = FALSE) +
    scale_fill_brewer(palette = "Set2") +
    guides(fill = "none") +
    labs(x = cn, y = nn,
      title    = sprintf(txt$title_by, nn, cn),
      subtitle = sub2,
      caption  = txt$cap_boxplot) +
    ds_theme_ex() +
    theme(legend.position = "none") +
    if (n_lvl > cfg$rot_thresh) ds_theme_rot() else NULL

  p_vl <- ggplot(df2, aes(x = g, y = v, fill = g)) +
    geom_violin(alpha = cfg$alpha, trim = FALSE, show.legend = FALSE) +
    geom_boxplot(width = 0.08, fill = "white",
      alpha = 0.85, show.legend = FALSE,
      outlier.shape = NA) +
    scale_fill_brewer(palette = "Set2") +
    guides(fill = "none") +
    labs(x = cn, y = nn, title = txt$title_violin) +
    ds_theme_ex() +
    theme(legend.position = "none") +
    if (n_lvl > cfg$rot_thresh) ds_theme_rot() else NULL

  p_ci <- ggplot(ci_data,
    aes(x = g, y = mw, ymin = lo, ymax = hi, color = g)) +
    geom_hline(yintercept = mean(num_v),
      linetype = "dashed", color = "grey55", linewidth = 0.5) +
    geom_pointrange(size = 0.65, linewidth = 0.9, show.legend = FALSE) +
    scale_color_brewer(palette = "Set2") +
    guides(color = "none") +
    labs(x = cn, y = nn,
      title    = txt$title_ci,
      subtitle = txt$sub_grand_mean) +
    ds_theme_ex() +
    theme(legend.position = "none") +
    if (n_lvl > cfg$rot_thresh) ds_theme_rot() else NULL

  # Three plots side by side; the wide group table spans the full width below,
  # its height growing with the number of groups.
  tbl_height <- 0.5 + 0.25 * n_lvl
  stats_tbl  <- fmt_group_table(grp_tbl, txt, cfg$sig_digits)
  (p_bx | p_vl | p_ci) / ev_stat_panel(stats_tbl) +
    plot_layout(heights = c(3, tbl_height)) & ev_ann_theme()
}

# Two categorical variables: grouped bar chart, contingency table, and a
# chi-squared test of independence (computed here, not by the describe engine).
ev_plot_cat_cat <- function(x_cc, y_cc, xname, yname, sub2, cfg) {
  txt <- cfg$txt
  fmt <- cfg$fmt

  df2   <- data.frame(x = droplevels(factor(x_cc)),
    y = droplevels(factor(y_cc)))
  n_lvl <- nlevels(df2$x)

  ct  <- table(df2$x, df2$y)
  chi <- suppressWarnings(chisq.test(ct))
  cap_cc <- paste0(
    sprintf(txt$cap_chisq, fmt(unname(chi$statistic)),
      as.integer(chi$parameter), fmt_p(chi$p.value)),
    if (any(chi$expected < 5)) txt$cap_chisq_warn else ""
  )

  p_bar2 <- ggplot(df2, aes(x = x, fill = y)) +
    geom_bar(position = "dodge", alpha = cfg$alpha,
      color = "white", linewidth = 0.2) +
    scale_fill_brewer(palette = "Set2") +
    labs(x = xname, fill = yname, y = txt$lbl_freq_bare,
      title    = sprintf(txt$title_freqcross, xname, yname),
      subtitle = sub2,
      caption  = cap_cc) +
    ds_theme_ex() +
    if (n_lvl > cfg$rot_thresh) ds_theme_rot() else NULL

  ct_mat <- as.data.frame.matrix(ct)
  ct_out <- cbind(data.frame(Cat = rownames(ct_mat), stringsAsFactors = FALSE),
    ct_mat)
  names(ct_out)[1] <- xname

  (p_bar2 | ev_stat_panel(ct_out)) +
    plot_layout(widths = c(3, 1.2)) & ev_ann_theme()
}

