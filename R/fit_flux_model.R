fit_flux_model <- function(flux_mod, co2_mod = NULL, ...) {
  UseMethod("fit_flux_model")
}

#' @exportS3Method MargLabGHG::fit_flux_model flux_mod
fit_flux_model.flux_mod <- function(flux_mod, co2_mod = NULL, ...) {
  result <- flux_mod
  # If CO2 model not specified, use all data
  if (is.null(co2_mod)) {
    result$ppm_processed <- result$ppm_raw
    result$seconds_processed <- result$seconds_raw

  # If CO2 model specified but processed data is NULL, fail and return
  } else if (is.null(co2_mod$ppm_processed)) {
    return(flux_mod_result(update = result, success = FALSE, reason = "Invalid CO2 data"))

  # Otherwise use CO2 processed data
  } else {
    result$seconds_processed <- co2_mod$seconds_processed
    result$ppm_processed <- with(result, ppm_raw[seconds_raw %in% seconds_processed])
  }
  result <- fail_if_co2_failed(result, co2_mod)
  result <- fail_if_obs_lt_min_n(result)
  model <- fit_linear_model(result)
  metrics <- get_model_metrics(model)
  result <- fail_if_low_r2(result, metrics)
  result$success <- if (is.null(result$success)) TRUE else result$success
  if (isTRUE(result$success) || result$force) {
    result <- flux_mod_result(
      update = result,
      models = list(linear = model),
      selected_model = "linear", metrics = metrics)
  }
  return(result)
}

#' @exportS3Method MargLabGHG::fit_flux_model flux_mod.CO2
fit_flux_model.flux_mod.CO2 <- function(
    flux_mod,
    ...
) {

  # Check for deadband options
  dots <- list(...)
  if (!"deadband_opts" %in% names(dots)) {
    warning("deadband_opts not specified for CO2 model - proceeding without deadband removal")
    return(fit_flux_model.flux_mod(flux_mod, ...))
  }

  # Dispatch to deadband removal
  deadband_opts <- dots$deadband_opts
  if (deadband_opts$method == "none") return(fit_flux_model.flux_mod(flux_mod, ...))
  deadband_fn <- paste0("apply_", deadband_opts$method, "_deadband_method")
  result <- do.call(deadband_fn, args = list(flux_mod = flux_mod, deadband_opts = deadband_opts))

  return(result)
}

#' @exportS3Method MargLabGHG::fit_flux_model flux_mod.N2O
fit_flux_model.flux_mod.N2O <- function(
    flux_mod, co2_mod = NULL,
    mod_opts = list(models = c("linear", "quadratic"),
                    selection_metric = c("RMSE", "R2", "none")),
    ...
) {
  result <- flux_mod

  if (!inherits(co2_mod, "flux_mod.CO2")) {
    warning("CO2 model result not specified for N2O modeling. CO2-identified deadband will not be removed.")
    result$seconds_processed <- result$seconds_raw
    result$ppm_processed <- result$ppm_raw

    # Check CO2 Model success
  } else if (!co2_mod$success) {

    # If CO2 model was not successful and this model is being forced, use raw
    # data
    if (result$force) {
    result$ppm_processed <- result$ppm_raw
    result$seconds_processed <- result$seconds_raw
    } else {
    # If CO2 model was not successful and this model is not being forced, return
      result$success <- FALSE
      result$reason <- "CO2 model was not successful"
      if (!result$force) return(result)
    }
  } else {
    # If CO2 model was suvvessful, use processed data from CO2 model
    result$seconds_processed <- co2_mod$seconds_processed
    result$ppm_processed <- with(result, ppm_raw[seconds_raw %in% seconds_processed])

  }

  # Fit both models
  models <- lapply(mod_opts$models, \(x) do.call(paste0("fit_", x, "_model"), list(result)))
  names(models) <- mod_opts$models

  # Get model metrics
  metrics <- lapply(models, get_model_metrics)

  # Select best model based on specified metric
  selection <- select_model(result, metrics)
  selected_model <- models[[selection]]
  selected_metrics <- metrics[[selection]]

  # Check R2 value against min R2
  result <- fail_if_low_r2(result, selected_metrics)

  # Make result
  success <- if (is.null(result$success)) TRUE else result$success
  if (isTRUE(success) || result$force) {
    result <- flux_mod_result(
      update = result, success = success, models = models,
      selected_model = selection, metrics = selected_metrics)
  }
  return(result)
}
