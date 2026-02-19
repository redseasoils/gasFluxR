#' Detect outliers
#'
#' @description Detects outliers in a vector as values outside a specified
#'   number of interquartile ranges below the 25% quantile or above the 75%
#'   quantile.
#'
#' @param x A numeric vector in which to detect outliers
#' @param n_IQR Integer. Number of interquartile ranges below 25% quantile or
#'   above 75% quantile at which a value will be considered an outlier. Defaults
#'   to 3.
#'
#' @return A logical vector the same length as `x`
#' @export
is_outlier <- function(x, n_IQR = 3) {
  {{x}} >= ((IQR({{x}}, na.rm = TRUE) * n_IQR) +
              quantile({{x}}, na.rm = TRUE)[4]) |
    {{x}} <= (quantile({{x}}, na.rm = TRUE)[2] -
                (IQR({{x}}, na.rm = TRUE) * n_IQR))
}
