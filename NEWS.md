# DSBtools 0.1.0

* First release. DSBtools provides teaching-oriented tools for the first steps of 
a data analysis, built around the scale of measurement of a variable (nominal, 
ordinal, discrete, or continuous). All output (plots, tables, and console messages) 
is available in English and German via the `language` argument or the option 
`DSBtools.language`.

## Data description

* `check_df()` checks every column of a data frame for typical pitfalls (category 
codes stored as numbers, counts stored as `double`, spelling variants, numbers 
stored as text, and outliers) and suggests how to correct them.
* `describe_var()` summarizes a single variable and `describe_df()` every column 
of a data frame as tidy tibbles, optionally grouped by one or more categorical 
variables. The statistics cover sample size, location, spread, shape (skewness 
and excess kurtosis), and inference (SE and 95% confidence interval).

## Data exploration

* `explore_var()` visualizes one or two variables with the plots appropriate for 
their scale of measurement, combined with a table of descriptive statistics. The 
type is detected from how a variable is stored in R and can be set manually via 
`x_type` and `y_type`.
* `explore_df()` applies `explore_var()` to every column of a data frame, optionally 
against a target variable.
* `check_normality()` assesses normality with a histogram, a Q-Q plot, skewness, 
excess kurtosis, and the Shapiro-Wilk test, and explains how to read the plots.
