#' Sample skewness (moment coefficient g1)
#'
#' @description
#' \deqn{g_1 = \frac{m_3}{m_2^{3/2}}, \quad
#'   m_k = \frac{1}{n}\sum_{i=1}^{n}(x_i - \bar{x})^k}
#'
#' @param x A numeric vector. Missing values are removed.
#' @return A single numeric value; `NA` if fewer than 3 values or zero variance.
#' @noRd
ds_skewness <- function(x) {
  x  <- x[!is.na(x)]          # remove missing values
  n  <- length(x)             # n
  if (n < 3) return(NA_real_)
  d  <- x - mean(x)           # deviations (x_i - x_bar)
  m2 <- sum(d^2) / n          # 2nd central moment (variance with 1/n)
  if (m2 == 0) return(NA_real_)
  m3 <- sum(d^3) / n          # 3rd central moment
  m3 / m2^(3/2)               # g1 = m3 / s^3
}

#' Sample excess kurtosis (moment coefficient g2)
#'
#' @description
#' \deqn{g_2 = \frac{m_4}{m_2^{2}} - 3, \quad
#'   m_k = \frac{1}{n}\sum_{i=1}^{n}(x_i - \bar{x})^k}
#'
#' @param x A numeric vector. Missing values are removed.
#' @return A single numeric value; `NA` if fewer than 4 values or zero variance.
#' @noRd
ds_kurtosis <- function(x) {
  x  <- x[!is.na(x)]          # remove missing values
  n  <- length(x)             # n
  if (n < 4) return(NA_real_)
  d  <- x - mean(x)           # deviations (x_i - x_bar)
  m2 <- sum(d^2) / n          # 2nd central moment (variance with 1/n)
  if (m2 == 0) return(NA_real_)
  m4 <- sum(d^4) / n          # 4th central moment
  m4 / m2^2 - 3               # g2 = m4 / s^4 - 3
}
