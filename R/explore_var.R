#' Explore one or two variables
#'
#' @description
#' `explore_var()` visualizes one or two variables with the plots that are
#' appropriate for their scale of measurement, combined with a table of
#' descriptive statistics. The type of each variable is detected automatically
#' and can be set manually if needed.
#'
#' @param x A vector (numeric, integer, character, logical, or factor).
#' @param y A vector of the same length as `x`, or `NULL` (default) for a
#'   one-variable analysis. For two numeric variables, `y` is treated as the
#'   response in the regression.
#' @param xlab A single character string for the x-axis label, or `NULL`
#'   (default) to use the expression passed as `x` (the part after `$`, if
#'   present).
#' @param ylab A single character string for the y-axis label, or `NULL`
#'   (default) to use the expression passed as `y` (the part after `$`, if
#'   present). Ignored if `y = NULL`.
#' @param x_type,y_type A single character string giving the type (scale of
#'   measurement) of `x` and `y`, one of `"nominal"`, `"ordinal"`, `"discrete"`,
#'   or `"continuous"` (partial matching is allowed, e.g., `"cont"`), or `NULL`
#'   (default) to detect the type automatically (see the *Variable types*
#'   section). Argument values are always given in English, independent of
#'   `language`.
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
#'   the language of all plot text elements, table labels, and console
#'   messages. Defaults to `getOption("DSBtools.language", "en")`.
#'
#' @details
#' The plots and statistics depend on the combination of variable types:
#'
#' | Combination | Output |
#' |-------------|--------|
#' | nominal / ordinal | bar chart and frequency table |
#' | discrete | bar chart with one bar per value (histogram with classes if more than `max_bars` values are possible), mean and median lines, 1-D boxplot, statistics table |
#' | continuous | histogram with density curve, mean and median lines, 1-D boxplot, statistics table |
#' | numeric x numeric | scatter plot with linear regression (95% CI) and LOESS smoother, regression equation, R², Pearson's *r* with p-value, marginal distributions, statistics table |
#' | categorical x numeric | boxplot with raw data and mean ± 95% CI, violin plot, group means ± 95% CI with grand mean, per-group statistics table |
#' | categorical x categorical | grouped bar chart, contingency table, chi-squared test of independence |
#'
#' The statistics table contains N, mean, median, SD, variance, IQR, min/max,
#' skewness, kurtosis, SE of the mean, and the two-sided 95% confidence
#' interval, which is always based on the *t*-distribution with *N* - 1 degrees
#' of freedom. The same statistics are returned as data by [describe_var()].
#'
#' Bar charts of nominal variables are sorted by frequency, those of ordinal
#' variables by level order. Missing values are removed; for two variables,
#' only complete pairs are used, and the number of removed pairs is reported in
#' the subtitle.
#'
#' @section Variable types:
#' R stores *how* data are encoded (character, factor, integer, double), but
#' not *what* the values mean. Hence, the scale of measurement cannot always be
#' derived from the data alone:
#' * category codes stored as numbers (e.g., `1` = control, `2` = treatment)
#'   look like discrete counts,
#' * counts stored as `double` look like continuous measurements,
#' * a character vector or unordered factor may in fact be ordinal.
#'
#' If `x_type` and `y_type` are `NULL` (default), `explore_var()` therefore
#' follows simple, transparent rules based only on how a variable is stored in
#' R:
#'
#' | Encoding in R | Detected type |
#' |---------------|---------------|
#' | `character`, `logical`, unordered `factor` | nominal |
#' | ordered `factor` (see [base::ordered()]) | ordinal |
#' | `integer` | discrete |
#' | `double` | continuous |
#'
#' The detected type is always reported in the console. In two ambiguous
#' situations, an additional hint suggests how to correct the type:
#' * an `integer` variable with few distinct values (10 or fewer) may contain
#'   category codes rather than counts,
#' * a `double` variable containing only whole numbers may contain counts and
#'   thus be discrete.
#'
#' @section Setting the type manually:
#' If the automatic detection does not match the meaning of a variable, there
#' are two ways to correct it:
#'
#' 1. **Encode the variable correctly in R** (recommended). Category codes
#'    become a factor, e.g., `factor(x, labels = c("control", "treatment"))`;
#'    ordinal data become an ordered factor, e.g.,
#'    `factor(x, levels = c("low", "medium", "high"), ordered = TRUE)`; and
#'    counts stored as `double` become integers with `as.integer(x)`.
#' 2. **Set the type explicitly** with `x_type` and `y_type`, e.g.,
#'    `x_type = "discrete"` for counts stored as `double`, or
#'    `x_type = "continuous"` for rounded measurements stored as `integer`
#'    (such as temperatures in whole degrees). The variable is converted
#'    internally; the original data remain unchanged.
#'
#' If a non-factor vector is set to `"ordinal"`, its levels are sorted
#' numerically or alphabetically, and a hint shows the resulting order. Setting
#' `"discrete"` or `"continuous"` requires numeric values, and `"discrete"`
#' additionally requires whole numbers; otherwise, an error is raised.
#'
#' @section Global options:
#' Some defaults can be set once per session (e.g., at the top of a course
#' script or in `.Rprofile`) instead of in every call:
#'
#' | Option | Argument | Default |
#' |--------|----------|---------|
#' | `DSBtools.language` | `language` | `"en"` |
#' | `DSBtools.max_bars` | `max_bars` | `30` |
#'
#' ```
#' options(DSBtools.language = "de", DSBtools.max_bars = 40)
#' ```
#'
#' Arguments passed directly to `explore_var()` always take precedence.
#'
#' @inheritSection describe_var Skewness and kurtosis
#'
#' @return
#' The function returns the composite [patchwork][patchwork::patchwork-package]
#' plot object invisibly; the figure is printed as a side effect. To obtain the
#' underlying statistics as data (numeric, unrounded, and with English column
#' names), use [describe_var()], which computes the same numbers.
#'
#' @references
#' Bulmer, M. G. (1979). *Principles of Statistics*. Dover Publications,
#' New York.
#'
#' Joanes, D. N., & Gill, C. A. (1998). Comparing measures of sample skewness
#' and kurtosis. *Journal of the Royal Statistical Society: Series D (The
#' Statistician)*, 47(1), 183–189. \doi{10.1111/1467-9884.00122}
#'
#' @seealso [describe_var()] for the statistics as data, and [base::factor()]
#'   and [base::ordered()] for encoding categorical variables.
#' @family data exploration functions
#'
#' @importFrom ggplot2 ggplot aes geom_histogram geom_density geom_bar
#'   geom_boxplot geom_violin geom_jitter geom_point geom_smooth geom_vline
#'   geom_hline geom_pointrange geom_text annotate labs
#'   theme_minimal theme element_text element_blank element_rect
#'   scale_x_continuous scale_y_continuous scale_color_brewer scale_fill_brewer
#'   after_stat unit guides
#' @importFrom patchwork plot_annotation plot_layout wrap_elements
#' @importFrom stats na.omit sd var median IQR qt cor cor.test lm coef
#'   complete.cases chisq.test
#' @importFrom utils globalVariables
#' @export
#'
#' @examples
#' # One variable: automatic type detection
#' explore_var(iris$Sepal.Length, xlab = "Sepal length (cm)")  # double -> continuous
#' explore_var(as.integer(InsectSprays$count),
#'             xlab = "Number of insects")                     # integer -> discrete
#' explore_var(iris$Species, xlab = "Species")                 # factor -> nominal
#' explore_var(esoph$agegp, xlab = "Age group")                # ordered -> ordinal
#'
#' # Setting the type manually
#' # Counts stored as double are detected as continuous (see console hint):
#' explore_var(InsectSprays$count, xlab = "Number of insects")
#' # ... set the type explicitly or convert with as.integer():
#' explore_var(InsectSprays$count, x_type = "discrete",
#'             xlab = "Number of insects")
#'
#' # Category codes stored as numbers: mtcars$am is coded 0/1
#' explore_var(mtcars$am, x_type = "nominal", xlab = "Transmission")
#' # ... or, better, encode it as a factor with meaningful labels:
#' explore_var(factor(mtcars$am, labels = c("automatic", "manual")),
#'             xlab = "Transmission")
#'
#' # Months are stored as integer (discrete), but they are ordered categories
#' explore_var(airquality$Month, x_type = "ordinal", xlab = "Month")
#'
#' # An unordered factor that is in fact ordinal (levels L < M < H)
#' explore_var(warpbreaks$tension, x_type = "ordinal", xlab = "Tension")
#'
#' # Two variables: numeric x numeric
#' explore_var(iris$Petal.Length, iris$Petal.Width,
#'             xlab = "Petal length (cm)", ylab = "Petal width (cm)")
#'
#' # Temperatures in whole degrees are stored as integer, but they are
#' # continuous measurements (Ozone contains missing values)
#' explore_var(airquality$Temp, airquality$Ozone,
#'             x_type = "continuous", y_type = "continuous",
#'             xlab = "Temperature (deg F)", ylab = "Ozone (ppb)")
#'
#' # Categorical x numeric; the dose (0.5, 1, 2 mg/day) is treated as ordinal
#' explore_var(ToothGrowth$dose, ToothGrowth$len, x_type = "ordinal",
#'             xlab = "Dose (mg/day)", ylab = "Tooth length")
#'
#' # Categorical x categorical
#' explore_var(warpbreaks$tension, warpbreaks$wool,
#'             xlab = "Tension", ylab = "Wool type")
#'
#' # German output
#' explore_var(iris$Species, iris$Petal.Length,
#'             xlab = "Art", ylab = "Kronblattlaenge (cm)",
#'             language = "de")
#'
#' # Set German for the whole session
#' old <- options(DSBtools.language = "de")
#' explore_var(mtcars$cyl, x_type = "discrete", xlab = "Anzahl Zylinder")
#' options(old)
explore_var <- function(x, y = NULL,
  xlab       = NULL,
  ylab       = NULL,
  x_type     = NULL,
  y_type     = NULL,
  max_bars   = getOption("DSBtools.max_bars", 30),
  bins       = NULL,
  color      = "#2E86AB",
  alpha      = 0.75,
  rot_thresh = 5,
  sig_digits = 3,
  language   = getOption("DSBtools.language", "en")) {

  xname <- xlab %||% strip_dollar(deparse1(substitute(x)))
  yname <- ylab %||%
    if (!is.null(y)) strip_dollar(deparse1(substitute(y))) else NULL

  language <- match.arg(language, c("en", "de"))
  if (!is.numeric(max_bars) || length(max_bars) != 1L || is.na(max_bars) ||
      max_bars < 1)
    stop("`max_bars` must be a single positive number.", call. = FALSE)
  if (!is.null(y) && length(y) != length(x))
    stop("`x` and `y` must have the same length.", call. = FALSE)

  x_type <- norm_type(x_type, "x_type")
  y_type <- norm_type(y_type, "y_type")

  txt <- ev_txt(language)

  # Shared drawing context passed to the plot helpers in
  # R/explore_var-helpers.R; they close over these graphical arguments.
  cfg <- list(color = color, alpha = alpha, bins = bins, max_bars = max_bars,
    rot_thresh = rot_thresh, sig_digits = sig_digits, txt = txt,
    fmt = function(v, sig = sig_digits) fmt_num(v, sig))

  # Resolve the type of x (messages and, if needed, conversion), then dispatch.
  rx    <- resolve_var(x, x_type, xname, "x_type", txt)
  x     <- rx$v
  xtype <- rx$type

  if (is.null(y)) {
    out <- if (ev_is_cat(xtype)) ev_plot_cat(x, xname, xtype, cfg)
    else                        ev_plot_num(x, xname, xtype, cfg)
    print(out)
    return(invisible(out))
  }

  ry    <- resolve_var(y, y_type, yname, "y_type", txt)
  y     <- ry$v
  ytype <- ry$type

  cc   <- complete.cases(x, y)
  x_cc <- x[cc]
  y_cc <- y[cc]
  sub2 <- ev_make_sub2(sum(cc), sum(!cc), txt)

  out <- if (!ev_is_cat(xtype) && !ev_is_cat(ytype))
    ev_plot_num_num(x_cc, y_cc, xname, yname, xtype, ytype, sub2, cfg)
  else if (xor(ev_is_cat(xtype), ev_is_cat(ytype)))
    ev_plot_cat_num(x_cc, y_cc, xtype, ytype, xname, yname, sub2, cfg)
  else
    ev_plot_cat_cat(x_cc, y_cc, xname, yname, sub2, cfg)

  print(out)
  invisible(out)
}
