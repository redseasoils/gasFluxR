#' Model gas flux from Gasmet data
#'
#' @description Model gas flux from Gasmet measurements. This function models
#'   raw gas concentrations over time and supports multiple deadband
#'   identification methods for CO2, CO2-determined deadband removal for all
#'   gases, and model selection (linear/quadratic) for N2O. Returns a
#'   comprehensive flux model object of class `flux_mod` and
#'   `flux_mod.<gas_name>`.
#'
#' @param data A data frame containing the time series of gas concentrations
#' @param ppm_var <[`data-masked`][rlang::args_data_masking]> Variable in
#'   `data` containing gas concentration values in parts per million or mg/L.
#' @param seconds_var <[`data-masked`][rlang::args_data_masking]> Variable in
#'   `data` containing time values in seconds (must start at 0)
#' @param co2_mod CO2 flux model object of class `flux_mod.CO2` (output of this
#'   function). Required when modeling gas fluxes that use CO2-determined
#'   deadband for data filtering.
#' @param min_n Minimum number of observations required in model. Default: 6
#' @param min_R2 Minimum R-squared value required for a model to be considered
#'   successful. Default: 0.98 for CO2, 0.1 for other gases.
#' @param gas_name Gas name: "CO2", "N2O", "CH4", or "NH3". If NULL (default),
#'   attempts to deduce from `ppm_var` column name (following Gasmet naming
#'   conventions).
#' @param force Logical. If `TRUE`, returns model results even when quality
#'   criteria (min_n, min_R2) are not met. Currently does not force data through
#'   modeling when deadband removal is deployed and the number of observations
#'   is < min_n, Default: FALSE
#' @param ... Additional arguments passed to gas-specific modeling methods. For
#'   CO2 models, can include `deadband_opts`. For N2O models, can include
#'   `mod_opts` list with `models` and `selection_metric` specifications. See
#'   `Details`.
#'
#' @return An object of class `flux_mod` (and `flux_mod.<gas_name>`) containing:
#'   \item{gas_name}{Gas species name}
#'   \item{ppm_raw, seconds_raw}{Original input data}
#'   \item{ppm_processed, seconds_processed}{Data after deadband removal}
#'   \item{min_n, min_R2}{Quality criteria used}
#'   \item{success}{Logical indicating if modeling was successful}
#'   \item{reason}{Character string explaining failure if not successful}
#'   \item{models}{List of fitted model objects (linear, quadratic)}
#'   \item{selected_model}{Name of the selected best model}
#'   \item{metrics}{Model performance metrics (R2, RMSE, AIC, BIC, etc.)}
#'   \item{deadband_info}{List with deadband removal method details}
#'   \item{force}{Whether force option was used}
#'
#' @details
#' ## Workflow
#'
#' 1. **Data validation**: Checks that seconds start at 0 and sufficient
#' non-NA/non-zero values exist
#'
#' 2. **Deadband removal** (CO2 only): Applies specified method to remove
#' initial chamber warm up
#'
#' 3. **Model fitting**:
#'
#'    - CO2: Linear model using deadband removal specifications
#'    - N2O: Linear and quadratic models using CO2-determined deadband, selects
#'           best by RMSE or R2
#'    - Other: Linear model using CO2-determined deadband (if `co2_mod` is
#'             specified) or using all data (if `co2_mod` is `NULL`)
#'
#' 4. **Quality assessment**: Checks against min_n and min_R2 criteria
#'
#' ## Deadband options for CO2 models
#'
#' A 'deadband' is a consecutive period of data in a chamber-based gas
#' measurement before which the chamber reaches equilibrium. Removing the
#' deadband is often desirable for greenhouse gas flux calculations, and is
#' typically easiest to identify using CO2 flux data due to its consistent
#' linear structure. Three methods of deadband removal for CO2 flux models are
#' currently supported:
#'
#'  1. **fixed**: Removes points at the beginning of a measurement occurring
#'                before a specified number of seconds.
#'  2. **minima**: Removes points before a minimum occurring within a certain
#'                 portion of number of seconds of measurement initiation. Good
#'                 for measurements characterized by an initial decrease in flux
#'                 followed by a linear increase.
#'  3. **optimum**: Algorithmically removes endpoints as needed by constructing
#'                  a linear model, comparing the R2 value to `min_R2`, and
#'                  removing the endpoint with the largest magnitude residual if
#'                  the R2 is inadequate. Endpoints are considered for removal
#'                  at both the beginning and end of the measurement if the
#'                  `trim_tails` option is set to `TRUE`, or only at the
#'                  beginning of the measurement if `FALSE` (the default; see
#'                  below). Endpoints will be removed so long as `min_R2` has
#'                  not been reached and at least `min_n` points remain.
#'                  Therefore, users should exercise caution in utilizing this
#'                  method, as it often leads to a larger identified deadband
#'                  than other methods, and thus less observations from which
#'                  the resulting model (and models of other gases based on the
#'                  CO2-identified deadband) are based.
#'
#' To apply deadband removal before finalizing the CO2 flux model, pass a
#' `deadband_opts` list via `...` with structure:
#'
#' ```
#' deadband_opts = list(
#'   method = c("fixed", "minima", "optimum", "none"),
#'   fixed = list(seconds = 30),           # Remove first N seconds
#'   minima = list(portion = 0.25,         # Remove up to first 25% of data
#'                 seconds = NULL),        # OR remove data before N seconds
#'   optimum = list(trim_tails = FALSE) # Iteratively remove points to maximize R2
#' )
#' ```
#'
#' To model CO2 flux data without deadband removal, simply do not pass
#' `deadband_opts`, or use `deadband_opts = list(method = "none")`.
#'
#' Note that `deadband_opts` is ignored for non-CO2 gases. If you'd like to
#' override this, you can 'trick' the function by setting `gas_name = "CO2"`,
#' while naming a different gas column in `gas_var`.
#'
#' ## Model selection options for N2O
#'
#' Linear and quadratic models are supported for N2O flux modeling, with model
#' selection based on either R2 or RMSE. To specify models desired and which
#' metric to use to choose a final model, pass a `mod_opts` list via `...` with
#' structure:
#'
#' ```
#' mod_opts = list(
#'   models = c("linear", "quadratic"),  # Models to fit
#'   selection_metric = c("RMSE", "R2")  # Metric for best model selection
#' )
#' ```
#'
#' **NOTE**: Currently, the `models` option is ignored, and both linear and
#' quadratic models are always fit to N2O flux data. This will be corrected
#' before v1.0 release.
#'
#' @seealso
#' * [calculate_gas_flux()] for converting model slopes to flux rates
#' * [import_gasmet_data()] for importing raw Gasmet data
#' * [import_chamber_volume()] for chamber dimension data
#'
#' @examples
#' \dontrun{
#' # Basic CO2 modeling
#' co2_result <- model_gas_flux(
#'   data = gasmet_data,
#'   ppm_var = Carbon.dioxide.CO2,
#'   seconds_var = seconds,
#'   gas_name = "CO2"
#' )
#'
#' # CO2 with custom deadband
#' co2_result <- model_gas_flux(
#'   data = gasmet_data,
#'   ppm_var = Carbon.dioxide.CO2,
#'   seconds_var = seconds,
#'   gas_name = "CO2",
#'   deadband_opts = list(
#'     method = "fixed",
#'     fixed = list(seconds = 45)
#'   )
#' )
#'
#' # N2O modeling using CO2 model for deadband
#' n2o_result <- model_gas_flux(
#'   data = gasmet_data,
#'   ppm_var = Nitrous.oxide.N2O,
#'   seconds_var = seconds,
#'   co2_mod = co2_result,  # from previous CO2 modeling
#'   gas_name = "N2O",
#'   mod_opts = list(
#'     models = c("linear", "quadratic"),
#'     selection_metric = "RMSE"
#'   )
#' )
#' }
#'
#' @export
model_gas_flux <- function(
    data, ppm_var, seconds_var, co2_mod = NULL,
    min_n = 6, min_R2 = ifelse(gas_name == "CO2", 0.98, 0.1), ...,
    gas_name = NULL, force = FALSE
) {

  # Guess gas name from ppm_var if not specified
  if (is.null(gas_name)) gas_name <- deduce_gas_name(data, {{ ppm_var }})

  # Get ppm and seconds as vectors
  df <- data %>% dplyr::arrange({{ seconds_var }})
  ppm <- df %>% dplyr::pull({{ ppm_var }})
  sec <- df %>% dplyr::pull({{ seconds_var }})

  # Initialize result structure
  mod <- flux_mod_result(gas_name = gas_name, ppm_raw = ppm, seconds_raw = sec,
                         min_n = min_n, min_R2 = min_R2, force = force)

  # Validate data. If data are invalid, return.
  # EM: Should this step return if force = TRUE? We could force a model with
  # < min_n observations, but I don't think that is as useful as other
  # force methods and would require reworking the deadband removal methods to
  # conditionally "ignore" min_n.
  validation <- validate_flux_mod_data(mod)
  if (!validation$valid) {
    mod <- flux_mod_result(update = mod, success = FALSE,
                           reason = validation$reason)
    return(mod)
  }
  # Execute modeling based on gas
  fit <- fit_flux_model(flux_mod = mod, co2_mod = co2_mod, ...)
  return(fit)
}

# Get simplified gas name from typical gasmet columns
#' @noRd
deduce_gas_name <- function(data, ppm_var) {
  ppm_name <- data %>% dplyr::select({{ ppm_var }}) %>% names()
  gas_name <- switch(ppm_name,
                     Carbon.dioxide.CO2 = "CO2",
                     Nitrous.oxide.N2O = "N2O",
                     Ammonia.NH3 = "NH3",
                     Methane.CH4 = "CH4",
                     Carbon.monoxide.CO = "CO",
                     Water.vapor.H2O = "H2O",
                     NA_character_)
  if (is.na(gas_name)) {
    stop(paste0("Gas name could not be deduced from ppm_var. Please specify ",
                "'gas_name'."))
  }
  return(gas_name)
}
