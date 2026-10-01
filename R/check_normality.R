#' Check a variable for normality
#'
#' @description
#' `check_normality()` draws the two plots most useful for judging normality of
#' a numeric variable side by side: a histogram with a fitted normal curve and a
#' normal quantile-quantile (Q-Q) plot. A one-line summary is printed to the
#' console, and a short reading guide is shown once per session.
#'
#' @param x A numeric vector.
#' @param xlab A single character string for the axis label, or `NULL` (default)
#'   to use the deparsed expression passed as `x`.
#' @param language A single character string, either `"en"` or `"de"`. Controls
#'   the language of all text. Defaults to `getOption("DSBtools.language", "en")`.
#' @param bins A single positive number giving the number of histogram bins, or
#'   `NULL` (default) for `max(8, ceiling(1 + log2(N)))`.
#' @param color A single character string with a valid R color for the histogram.
#'   Defaults to `"#2E86AB"`.
#' @param alpha A single number in `[0, 1]` giving the fill transparency. Defaults
#'   to `0.75`.
#'
#' @details
#' Many classical tests assume approximately normal data, and this function helps
#' you check that visually.
#'
#' The **histogram** uses the *density* scale (not counts, unlike
#' [explore_var()]) so that the fitted normal curve is directly comparable: the
#' solid curve is the smoothed data density and the red dashed curve is a normal
#' distribution with the same mean and standard deviation. If the two curves
#' roughly agree and the shape is a symmetric bell, the data are close to normal.
#'
#' The **Q-Q plot** compares the ordered data with the values expected from a
#' normal distribution; its reference line passes through the 1st and 3rd
#' quartiles of the data. Points on the line indicate normality; a systematic
#' curve indicates skew, and an S-shape indicates heavy or light tails. For an
#' integer variable with few distinct values, expect visible steps.
#'
#' The caption reports the moment coefficients of skewness (\eqn{g_1}) and
#' excess kurtosis (\eqn{g_2}); both are about 0 for normal data (see
#' [describe_var()]). When `3 <= N <= 5000`, the Shapiro-Wilk statistic is also
#' reported. Treat the p-value with care: with large samples even tiny,
#' unimportant deviations become "significant", so the plots should carry more
#' weight.
#'
#' @return The composite [patchwork][patchwork::patchwork-package] plot object,
#'   returned invisibly. The figure is printed as a side effect.
#'
#' @seealso [explore_var()], [describe_var()]
#'
#' @references
#' Joanes, D. N., & Gill, C. A. (1998). Comparing measures of sample skewness
#' and kurtosis. *Journal of the Royal Statistical Society: Series D (The
#' Statistician)*, 47(1), 183-189. \doi{10.1111/1467-9884.00122}
#'
#' Shapiro, S. S., & Wilk, M. B. (1965). An analysis of variance test for
#' normality (complete samples). *Biometrika*, 52(3-4), 591-611.
#' \doi{10.1093/biomet/52.3-4.591}
#'
#' @importFrom ggplot2 stat_function stat_qq stat_qq_line
#' @importFrom stats dnorm shapiro.test
#' @export
#'
#' @examples
#' # roughly normal
#' check_normality(iris$Sepal.Length, xlab = "Sepal length (cm)")
#'
#' # right-skewed
#' check_normality(airquality$Ozone, xlab = "Ozone (ppb)")
check_normality <- function(x,
  xlab     = NULL,
  language = getOption("DSBtools.language", "en"),
  bins     = NULL,
  color    = "#2E86AB",
  alpha    = 0.75) {

  language <- match.arg(language, c("en", "de"))
  xname    <- xlab %||% deparse1(substitute(x))
  if (!is.numeric(x))
    stop("`x` must be numeric.", call. = FALSE)

  txt    <- cn_txt(language)
  n_miss <- sum(is.na(x))
  v      <- as.numeric(x)
  v      <- v[!is.na(v)]
  if (length(v) < 3)
    stop("`x` must have at least 3 non-missing values.", call. = FALSE)
  if (sd(v) == 0)
    stop(sprintf(txt$err_zero_var, xname), call. = FALSE)

  mu  <- mean(v)
  sdv <- sd(v)
  g1  <- ds_skewness(v)
  g2  <- ds_kurtosis(v)
  nb  <- if (!is.null(bins)) bins else max(8L, ceiling(1 + log2(length(v))))
  df  <- data.frame(x = v)

  p_hist <- ggplot(df, aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)), bins = nb,
      fill = color, alpha = alpha, color = "white", linewidth = 0.2) +
    geom_density(color = scales::muted(color, l = 40),
      linewidth = 1, adjust = 1.2) +
    stat_function(fun = dnorm, args = list(mean = mu, sd = sdv),
      color = "#E74C3C", linewidth = 0.9, linetype = "dashed") +
    labs(x = xname, y = txt$lbl_density,
      title = txt$title_hist, subtitle = txt$sub_hist) +
    ds_theme_ex()

  p_qq <- ggplot(df, aes(sample = x)) +
    stat_qq(color = color, alpha = 0.6, size = 1.6) +
    stat_qq_line(color = "#E74C3C", linewidth = 0.9) +
    labs(x = txt$lbl_theo, y = txt$lbl_sample,
      title = txt$title_qq, subtitle = txt$sub_qq) +
    ds_theme_ex()

  sh <- if (length(v) >= 3 && length(v) <= 5000)
    suppressWarnings(shapiro.test(v)) else NULL

  # -- Caption: skewness/kurtosis always; Shapiro-Wilk appended if available ----
  cap <- sprintf(txt$cap_moments, sprintf("%.2f", g1), sprintf("%.2f", g2))
  if (!is.null(sh))
    cap <- paste0(cap, "  |  ",
      sprintf(txt$cap_shapiro, fmt_num(unname(sh$statistic), 3), fmt_p(sh$p.value)))

  # -- Console: one-line header; reading guide only once per session ------------
  rlang::inform(sprintf(txt$msg_head, xname, length(v), n_miss))
  if (is.integer(x) && n_distinct(x) <= 10)
    rlang::inform(sprintf(txt$hint_discrete, xname))
  if (is.null(.ds_env$normality_guide_shown)) {
    cli::cli_text(txt$guide_head)
    cli::cli_ul(txt$guide)
    cli::cli_text("{.emph {txt$guide_once}}")
    .ds_env$normality_guide_shown <- TRUE
  }

  out <- (p_hist | p_qq) +
    plot_annotation(
      title   = sprintf(txt$title_main, xname),
      caption = cap,
      theme   = theme(
        plot.title      = element_text(face = "bold", size = 13),
        plot.caption    = element_text(hjust = 0, color = "grey45"),
        plot.background = element_rect(fill = "white", color = NA))
    )
  print(out)
  invisible(out)
}

# Translations for check_normality().
cn_txt <- function(language) {
  if (language == "de") list(
    title_main  = "Normalit\u00e4tscheck: %s",
    title_hist  = "Histogramm mit Normalverteilung",
    sub_hist    = "Durchgezogen = Dichte der Daten  |  Rot gestrichelt = Normalverteilung",
    title_qq    = "Q-Q-Diagramm (Normalverteilung)",
    sub_qq      = "Punkte auf der Linie = normal",
    lbl_density = "Dichte",
    lbl_theo    = "Theoretische Quantile",
    lbl_sample  = "Beobachtete Quantile",
    msg_head    = "[check_normality] '%s': N = %d (%d fehlend)",
    guide_head  = "So liest du die Grafiken:",
    guide_once  = "Dieser Hinweis erscheint einmal pro Sitzung.",
    guide = c(
      "Histogramm: eine symmetrische Glockenform um den Mittelwert spricht f\u00fcr Normalit\u00e4t; ein langer Ausl\u00e4ufer auf einer Seite zeigt Schiefe.",
      "Q-Q-Diagramm: Punkte nahe der Linie = normal; eine S-Form deutet auf schwere/leichte Enden hin, eine gebogene Kurve auf Schiefe.",
      "Achtung: Bei kleinem N k\u00f6nnen auch normale Daten unregelm\u00e4\u00dfig aussehen."),
    cap_moments = "g1 = %s, g2 = %s",
    cap_shapiro = "Shapiro-Wilk: W = %s, p %s",
    err_zero_var = "'%s' hat keine Varianz (alle Werte sind gleich); ein Normalit\u00e4tscheck ist nicht sinnvoll.",
    hint_discrete = "[check_normality] '%s' ist ganzzahlig mit wenigen verschiedenen Werten; Stufen im Q-Q-Diagramm sind bei diskreten Daten zu erwarten."
  ) else list(
    title_main  = "Normality check: %s",
    title_hist  = "Histogram with normal curve",
    sub_hist    = "Solid = data density  |  Red dashed = normal distribution",
    title_qq    = "Q-Q plot (normal)",
    sub_qq      = "Points on the line = normal",
    lbl_density = "Density",
    lbl_theo    = "Theoretical quantiles",
    lbl_sample  = "Sample quantiles",
    msg_head    = "[check_normality] '%s': N = %d (%d missing)",
    guide_head  = "How to read the plots:",
    guide_once  = "This hint is shown once per session.",
    guide = c(
      "Histogram: a symmetric bell shape around the mean suggests normality; a long tail to one side indicates skew.",
      "Q-Q plot: points close to the line = normal; an S-shape suggests heavy/light tails, a bent curve suggests skew.",
      "Note: with a small N even normal data can look irregular."),
    cap_moments = "g1 = %s, g2 = %s",
    cap_shapiro = "Shapiro-Wilk: W = %s, p %s",
    err_zero_var = "'%s' has zero variance (all values are equal); a normality check is not meaningful.",
    hint_discrete = "[check_normality] '%s' is integer with few distinct values; steps in the Q-Q plot are expected for discrete data."
  )
}
