# Shared internal helpers used by several DSBtools functions.
#
# Not exported. Package-wide imports (ggplot2, ...) are declared in the roxygen
# block of explore_var() in R/explore_var.R and apply to the whole namespace.

# Null-coalescing operator (base R gained `%||%` only in 4.4.0; define our own
# so the package works from R 4.0.0).
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Common minimal theme used across all DSBtools figures.
ds_theme_ex <- function(base = 11) {
  theme_minimal(base_size = base) +
    theme(
      plot.title       = element_text(face = "bold", size = base + 1),
      plot.subtitle    = element_text(size = base - 1, color = "grey55"),
      plot.caption     = element_text(size = base - 2, color = "grey65",
        hjust = 0),
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "white", color = NA)
    )
}

# 45-degree x-axis labels, for categorical axes with many levels.
ds_theme_rot <- function() {
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
}

# Build a themed table grob (grid object) from a data.frame, with the standard
# alternating (zebra) row shading.
ds_tablegrob_grob <- function(df, fontsize = 9.5) {
  gridExtra::tableGrob(
    df, rows = NULL,
    theme = gridExtra::ttheme_minimal(
      core    = list(fg_params = list(fontsize = fontsize),
        bg_params = list(fill = c("grey98", "white"), col = NA)),
      colhead = list(fg_params = list(fontsize = fontsize, fontface = "bold"),
        bg_params = list(fill = "grey90", col = NA))
    )
  )
}

# Render a data.frame as a themed table grob, wrapped as a patchwork element.
ds_tablegrob <- function(df, fontsize = 9.5) {
  wrap_elements(ds_tablegrob_grob(df, fontsize))
}

# Localised label for a detected type, given a language code ("en"/"de").
ds_type_label <- function(typ, language) type_label(typ, ev_txt(language))

# Standard white-background annotation theme for composite (patchwork) figures.
ds_ann_theme <- function() {
  plot_annotation(
    theme = theme(plot.background = element_rect(fill = "white", color = NA))
  )
}
