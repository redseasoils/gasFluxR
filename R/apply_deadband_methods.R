# === === === === === === === === === === === === === === === === === === === ==
# Deadband Removal Methods ----
# === === === === === === === === === === === === === === === === === === === ==

#' Apply fixed deadband removal method
#' @noRd
apply_fixed_deadband_method <- function(flux_mod, deadband_opts) {

  result <- flux_mod
  if (is.null(result$force)) result$force <- FALSE
  fixed_seconds <- deadband_opts$fixed$seconds
  if (is.null(fixed_seconds)) fixed_seconds <- 30 # default to 30 if unspecified

  # Remove observations within fixed deadband
  keep_idx <- result$seconds_raw > fixed_seconds
  result$ppm_processed <- result$ppm_raw[keep_idx]
  result$seconds_processed <- result$seconds_raw[keep_idx]

  # Check if n obs >= min_n after deadband removal
  result <- update_success_min_n(result)

  # Fit model
  model <- fit_linear_model(result)
  metrics <- get_model_metrics(model)

  # Check R2 against min_R2
  result <- update_success_r2(result, metrics)

  # Compile info for result
  success <- if (is.null(result$success)) TRUE else result$success
  reason <- if (is.null(result$reason)) NULL else result$reason
  deadband_info <- list(
    method = "fixed",
    removed_n = sum(!keep_idx),
    removed_idx = which(!keep_idx),
    fixed_seconds = fixed_seconds
  )

  # Make result
  if (isTRUE(success) || result$force) {
    result <- flux_mod_result(
      update = result, success = success, models = list(linear = model),
      selected_model = "linear",
      metrics = metrics, deadband_info = deadband_info)
  } else {
    result <- flux_mod_result(
      update = result, success = success, reason = reason,
      deadband_info = deadband_info)
  }

  return(result)
}

#' Apply minima-based deadband removal method
#' @noRd
apply_minima_deadband_method <- function(flux_mod, deadband_opts) {

  result <- flux_mod
  minima_seconds <- deadband_opts$minima$seconds
  minima_portion <- deadband_opts$minima$portion
  if (is.null(minima_seconds) && is.null(minima_portion)) {
    minima_portion <- 0.25 # default if neither option is specified
  }

  # Determine search range for minimum
  if (!is.null(minima_seconds)) {
    search_range <- which(result$seconds_raw < minima_seconds)
  } else {
    search_end <- ceiling(length(result$seconds_raw) * minima_portion)
    search_range <- 1:search_end
  }

  search_ppm <- result$ppm_raw[search_range]

  if (length(search_ppm) == 0 || all(is.na(search_ppm))) {
    result$success <- FALSE
    result$reason <- "No data points in specified minima search range"
    result$deadband_info <- list(
      method = "minima", removed_n = 0, removed_idx = NULL,
      minima_seconds = minima_seconds, minima_portion = minima_portion)
    return(result)
  }

  # Find minimum
  min_idx <- which.min(search_ppm)

  # Remove data before minimum
  result$ppm_processed <- result$ppm_raw[min_idx:length(result$ppm_raw)]
  result$seconds_processed <- result$seconds_raw[min_idx:length(result$seconds_raw)]

  # Check processed data vs min_n
  result <- update_success_min_n(result)

  # Fit model
  model <- fit_linear_model(result)
  metrics <- get_model_metrics(model)

  # Check R2 against min_R2
  result <- update_success_r2(result, metrics)

  # Compile info for result
  success <- if (is.null(result$success)) TRUE else result$success
  reason <- if (is.null(result$reason)) NULL else result$reason
  deadband_info <- list(
    method = "minima",
    removed_n = min_idx - 1,
    removed_idx = if (min_idx == 1) NULL else 1:min_idx,
    minima_seconds = minima_seconds,
    minima_portion = minima_portion
  )

  # Make result
  if (isTRUE(success) || result$force) {
    result <- flux_mod_result(
      update = result, success = success, models = list(linear = model),
      selected_model = if (isTRUE(success)) "linear",
      metrics = metrics, deadband_info = deadband_info)
  } else {
    result <- flux_mod_result(
      update = result, success = success, reason = reason,
      deadband_info = deadband_info)
  }

  return(result)
}

#' Apply optimum method
#' @noRd
apply_optimum_deadband_method <- function(flux_mod, deadband_opts) {

  result <- flux_mod
  trim_tails <- deadband_opts$optimum$trim_tails
  if (is.null(trim_tails)) trim_tails <- FALSE # default to FALSE if unspecified
  df <- data.frame(ppm = result$ppm_raw, sec = result$seconds_raw)

  # For optimum, we always try to find the best model
  current_df <- df
  best_model <- NULL
  best_metrics <- NULL
  best_df <- NULL
  iterations <- 0

  while (nrow(current_df) >= result$min_n && !isTRUE(result$success)) {
    model <- fit_linear_model(x = current_df$sec, y = current_df$ppm)
    metrics <- get_model_metrics(model)

    # Keep track of the best model we've seen
    if (is.null(best_model) || isTRUE(metrics$r_squared > best_metrics$r_squared)) {
      best_model <- model
      best_metrics <- metrics
      best_df <- current_df
    }

    if (isTRUE(metrics$r_squared >= result$min_R2)) result$success <- TRUE

    # Remove point for next iteration
    if (trim_tails) {
      # Remove endpoint with largest residual
      residuals <- abs(resid(model))
      if (residuals[1] >= residuals[length(residuals)]) {
        current_df <- current_df[-1, ]
      } else {
        current_df <- current_df[-nrow(current_df), ]
      }
    } else {
      # Remove first point
      current_df <- current_df[-1, ]
    }
    iterations <- iterations + 1
  }

  if (!isTRUE(result$success)) { # if min R2 not achieved
    result$success <- FALSE
    result$reason <- paste0("Could not achieve R2 ≥ ", result$min_R2,
                            " with at least ", result$min_n, " observations")
  }

  if (isTRUE(result$success) || result$force) {
    result$ppm_processed <- best_df$ppm
    result$seconds_processed <- best_df$sec
    result$models <- list(linear = best_model)
    if (isTRUE(result$success)) result$selected_model <- "linear"
    result$metrics <- best_metrics
    result$deadband_info <- list(
      method = "optimum",
      removed_points_n = nrow(df) - nrow(best_df),
      removed_points_idx = which(!df$sec %in% best_df$sec)
    )
  } else {
    result$ppm_processed <- result$ppm_raw
    result$seconds_processed <- result$seconds_raw
    result$deadband_info <- list(method = "optimum")
  }
  return(result)
}

# Update success and reason entries for flux_mod after deadband removal,
# checking the number of remaining observations against min_n
#' @noRd
update_success_min_n <- function(flux_mod) {
  min_n_fail <- length(flux_mod$ppm_processed) < flux_mod$min_n
  if (min_n_fail) {
    success <- FALSE
    reason <- paste0("After removing deadband, ",
                     length(flux_mod$ppm_processed), " obs < min_n (",
                     flux_mod$min_n, ")")
    result <- flux_mod_result(
      update = flux_mod, success = success, reason = reason)
    return(result)
  } else {
    return(flux_mod)
  }
}

# Update success and reason entries for flux_mod after modeling,
# checking the R2 value against min_R2
#' @noRd
update_success_r2 <- function(flux_mod, metrics) {
  r2_fail <- is.na(metrics$r_squared) || isTRUE(metrics$r_squared < flux_mod$min_R2)
  if (r2_fail) {
    success <- FALSE
    reason <- paste0("R² (", round(metrics$r_squared, 3), ") < ", flux_mod$min_R2)
    result <- flux_mod_result(
      update = flux_mod, success = success, reason = reason)
    return(result)
  } else {
    return(flux_mod)
  }
}
