# Internal helpers for which_test(): decision logic and translations.
# Not exported. All strings are ASCII; non-ASCII uses \uXXXX escapes.

# ---------------------------------------------------------------------------
# Decision logic (language-independent)
# ---------------------------------------------------------------------------

# Given two vectors, return a list describing the recommended test:
#   x_type, y_type, scenario ("assoc_num"/"groups"/"cat_cat"),
#   n_groups, cat_ordinal, small_expected, expected_min, test, alt
wt_suggest <- function(x, y) {
  tx <- detect_type(x)
  ty <- detect_type(y)
  is_num <- function(t) t %in% c("discrete", "continuous")
  is_cat <- function(t) t %in% c("nominal", "ordinal")
  is_ord <- function(t) t == "ordinal"

  res <- list(x_type = tx, y_type = ty, n_groups = NA_integer_,
    cat_ordinal = FALSE, small_expected = FALSE, expected_min = NA_real_)

  # numeric vs numeric -> correlation
  if (is_num(tx) && is_num(ty)) {
    res$scenario <- "assoc_num"
    if (is_ord(tx) || is_ord(ty)) {
      res$test <- "spearman"; res$alt <- "pearson"
    } else {
      res$test <- "pearson";  res$alt <- "spearman"
    }
    return(res)
  }

  # exactly one categorical -> group comparison
  if (is_cat(tx) != is_cat(ty)) {
    res$scenario <- "groups"
    cat_ord <- if (is_cat(tx)) is_ord(tx) else is_ord(ty)
    catv    <- if (is_cat(tx)) x else y
    k <- nlevels(droplevels(factor(catv[!is.na(catv)])))
    res$n_groups    <- k
    res$cat_ordinal <- cat_ord
    if (k < 2) {
      res$test <- "none_one_group"; res$alt <- NA_character_
    } else if (k == 2) {
      if (cat_ord) { res$test <- "mannwhitney"; res$alt <- "ttest" }
      else         { res$test <- "ttest";       res$alt <- "mannwhitney" }
    } else {
      if (cat_ord) { res$test <- "kruskal"; res$alt <- "anova" }
      else         { res$test <- "anova";   res$alt <- "kruskal" }
    }
    return(res)
  }

  # categorical vs categorical -> chi-squared / Fisher
  res$scenario <- "cat_cat"
  cc  <- complete.cases(x, y)
  tb  <- table(x[cc], y[cc])
  em  <- tryCatch(min(suppressWarnings(chisq.test(tb)$expected)),
    error = function(e) NA_real_)
  res$expected_min   <- em
  res$small_expected <- is.finite(em) && em < 5
  if (isTRUE(res$small_expected)) { res$test <- "fisher"; res$alt <- "chisq" }
  else                            { res$test <- "chisq";  res$alt <- "fisher" }
  res
}

# ---------------------------------------------------------------------------
# Translations
# ---------------------------------------------------------------------------

wt_txt <- function(language) {
  if (language == "de") list(
    hdr           = "[which_test] Testempfehlung",
    lbl_types     = "Erkannte Typen: x = %s, y = %s",
    lbl_scenario  = "Situation:",
    lbl_test      = "Empfohlener Test: %s",
    lbl_assump    = "Voraussetzungen (bitte pr\u00fcfen):",
    lbl_alt       = "Falls Voraussetzungen verletzt sind: %s",
    reason_assoc  = "Beide Variablen sind numerisch - es geht um den Zusammenhang zweier quantitativer Gr\u00f6\u00dfen.",
    reason_groups = "Eine numerische Variable wird \u00fcber %d Gruppen verglichen.",
    reason_catcat = "Beide Variablen sind kategorial - getestet wird, ob sie voneinander abh\u00e4ngen (Unabh\u00e4ngigkeit).",
    note_ordinal  = "Hinweis: Mindestens eine Variable ist ordinal - ein rangbasierter (nicht-parametrischer) Test ist meist besser geeignet.",
    note_small_expected = "Hinweis: Einige erwartete H\u00e4ufigkeiten sind < 5 - die Chi-Quadrat-N\u00e4herung ist unzuverl\u00e4ssig, daher Fishers exakter Test.",
    note_one_group = "Die kategoriale Variable hat weniger als zwei Gruppen - ein Gruppenvergleich ist nicht m\u00f6glich.",
    tests = list(
      pearson = list(
        name   = "Pearson-Korrelation / lineare Regression",
        call   = "cor.test(x, y)   # oder lm(y ~ x)",
        assump = c("linearer Zusammenhang", "ann\u00e4hernd normalverteilte Residuen",
                   "keine starken Ausrei\u00dfer")),
      spearman = list(
        name   = "Spearman-Rangkorrelation",
        call   = "cor.test(x, y, method = \"spearman\")",
        assump = c("monotoner Zusammenhang", "ordinale oder nicht-normale Daten")),
      ttest = list(
        name   = "Zwei-Stichproben-t-Test (Welch)",
        call   = "t.test(y ~ x)",
        assump = c("unabh\u00e4ngige Beobachtungen",
                   "ann\u00e4hernd normalverteilte Gruppen (oder gro\u00dfes n)")),
      mannwhitney = list(
        name   = "Mann-Whitney-U-Test (Wilcoxon-Rangsummentest)",
        call   = "wilcox.test(y ~ x)",
        assump = c("unabh\u00e4ngige Beobachtungen", "ordinale oder nicht-normale Daten")),
      anova = list(
        name   = "Einfaktorielle Varianzanalyse (ANOVA)",
        call   = "summary(aov(y ~ x))",
        assump = c("unabh\u00e4ngige Beobachtungen", "ann\u00e4hernd normalverteilte Residuen",
                   "\u00e4hnliche Gruppenvarianzen")),
      kruskal = list(
        name   = "Kruskal-Wallis-Test",
        call   = "kruskal.test(y ~ x)",
        assump = c("unabh\u00e4ngige Beobachtungen", "ordinale oder nicht-normale Daten")),
      chisq = list(
        name   = "Chi-Quadrat-Unabh\u00e4ngigkeitstest",
        call   = "chisq.test(table(x, y))",
        assump = c("unabh\u00e4ngige Beobachtungen",
                   "erwartete H\u00e4ufigkeiten >= 5 in den meisten Zellen")),
      fisher = list(
        name   = "Fishers exakter Test",
        call   = "fisher.test(table(x, y))",
        assump = c("unabh\u00e4ngige Beobachtungen", "geeignet f\u00fcr kleine erwartete H\u00e4ufigkeiten"))
    )
  ) else list(
    hdr           = "[which_test] Test suggestion",
    lbl_types     = "Detected types: x = %s, y = %s",
    lbl_scenario  = "Situation:",
    lbl_test      = "Recommended test: %s",
    lbl_assump    = "Assumptions (please check):",
    lbl_alt       = "If assumptions are violated: %s",
    reason_assoc  = "Both variables are numeric - you are looking at the association between two quantitative variables.",
    reason_groups = "A numeric variable is compared across %d groups.",
    reason_catcat = "Both variables are categorical - you are testing whether they are associated (independence).",
    note_ordinal  = "Note: at least one variable is ordinal - a rank-based (non-parametric) test is usually more appropriate.",
    note_small_expected = "Note: some expected counts are < 5 - the chi-squared approximation is unreliable, so Fisher's exact test is preferred.",
    note_one_group = "The categorical variable has fewer than two groups - a group comparison is not possible.",
    tests = list(
      pearson = list(
        name   = "Pearson correlation / linear regression",
        call   = "cor.test(x, y)   # or lm(y ~ x)",
        assump = c("linear relationship", "roughly normal residuals",
                   "no strong outliers")),
      spearman = list(
        name   = "Spearman rank correlation",
        call   = "cor.test(x, y, method = \"spearman\")",
        assump = c("monotonic relationship", "ordinal or non-normal data")),
      ttest = list(
        name   = "Two-sample t-test (Welch)",
        call   = "t.test(y ~ x)",
        assump = c("independent observations",
                   "approximately normal groups (or large n)")),
      mannwhitney = list(
        name   = "Mann-Whitney U test (Wilcoxon rank-sum)",
        call   = "wilcox.test(y ~ x)",
        assump = c("independent observations", "ordinal or non-normal data")),
      anova = list(
        name   = "One-way ANOVA",
        call   = "summary(aov(y ~ x))",
        assump = c("independent observations", "approximately normal residuals",
                   "similar group variances")),
      kruskal = list(
        name   = "Kruskal-Wallis test",
        call   = "kruskal.test(y ~ x)",
        assump = c("independent observations", "ordinal or non-normal data")),
      chisq = list(
        name   = "Chi-squared test of independence",
        call   = "chisq.test(table(x, y))",
        assump = c("independent observations",
                   "expected counts >= 5 in most cells")),
      fisher = list(
        name   = "Fisher's exact test",
        call   = "fisher.test(table(x, y))",
        assump = c("independent observations", "suitable for small expected counts"))
    )
  )
}
