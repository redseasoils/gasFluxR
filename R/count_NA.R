#' Print a message with count and percent NAs in a numeric vector
#'
#' @param x A numeric vector
#'
#' @returns Nothing
#' @export
#'
#' @examples
#' count_NA(c(1, NA, 2, 3, 4, NA))
count_NA <- function(x) {
  len <- length(x)
  na <- sum(is.na(x))
  pct <- (na / len) * 100
  rnd <- ifelse(pct < 1, 2, 0)
  pct <- sprintf(paste0('%.', rnd, "f"), round(pct, rnd))
  cat(sprintf("Out of %s, %s are NA (%s%s)", len, na, pct, "%"))
}
