#' Validate data for flux modeling
#' @noRd
validate_flux_mod_data <- function(flux_mod, allow_all_zero = FALSE) {
  ppm <- flux_mod$ppm_raw
  seconds <- flux_mod$seconds_raw
  min_n <- flux_mod$min_n

  # Check that seconds start at 0
  if (seconds[1] != 0) {
    return(list(
      valid = FALSE,
      possible = FALSE,
      reason = "`seconds` must start at 0"
    ))
  }

  # Check minimum non-NA values
  non_na <- sum(!is.na(ppm) & !is.na(seconds))
  if (non_na < 2) {
    return(list(valid = FALSE, possible = TRUE, reason = "<2 non-NA values"))
  }
  if (non_na < min_n) {
    return(list(
      valid = FALSE, possible = TRUE,
      reason = paste0("Non-NA values (", non_na, ") < min_n (", min_n, ")")
    ))
  }

  # Check minimum non-zero values
  if (!allow_all_zero) {
    non_zero <- sum(!ppm == 0, na.rm = TRUE)
    if (non_zero < min_n) {
      return(list(
        valid = FALSE, possible = TRUE,
        reason = paste0("Non-zero values (", non_zero, ") < min_n (", min_n, ")")
      ))
    }
  }

  return(list(valid = TRUE, possible = TRUE, reason = NULL))
}
