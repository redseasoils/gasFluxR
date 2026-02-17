#' Calculate gas flux rates from linear model slopes
#'
#' @description Calculates gas flux rates (kg ha⁻¹ d⁻¹) from flux model
#'   slopes (ppm per second) using chamber measurements and the ideal gas law.
#'   Supports CO₂, N₂O, CH₄, and NH₃ flux calculations and converts units to
#'   C- or N-basis.
#'
#' @param slope Slope from linear model (ppm per second)
#' @param gas_name Name of the gas: "CO2", "N2O", "CH4", or "NH3"
#' @param chamber_temp_c Chamber temperature in degrees Celsius
#' @param chamber_height_cm "Chamber height" (i.e. volume:area ratio) in cm
#' @param calculation_type Type of calculation method. Currently only "Gasmet"
#'   is supported. Defaults to "Gasmet".
#'
#' @details The flux calculation follows the formula:
#'   \deqn{f_m = \alpha_m \times M_m \times \frac{1}{RT} \times \frac{V}{A} \times \frac{1}{1000} \times 10000 \times 3600 \times p \times 0.00024}
#'   Where:
#'   \itemize{
#'     \item \eqn{f_m} = flux (µg m⁻² hr⁻¹)
#'     \item \eqn{\alpha_m} = slope (ppm second⁻¹)
#'     \item \eqn{M_m} = molar mass (µg µmol⁻¹)
#'     \item \eqn{R} = ideal gas constant (0.0821 L·atm·K⁻¹·mol⁻¹)
#'     \item \eqn{T} = temperature (K)
#'     \item \eqn{V/A} = chamber height (cm)
#'     \item \eqn{p} = proportion of C or N by weight
#'   }
#'
#'   Unit conversions:
#'   \itemize{
#'     \item ppm = µL trace gas L⁻¹ total gas
#'     \item slope = µL trace gas L⁻¹ total gas second⁻¹
#'     \item 1 L = 1000 cm³
#'     \item 1 m² = 10,000 cm²
#'     \item 1 hr = 3600 seconds
#'     \item 1 ug m⁻² h⁻¹ = 0.00024 kg ha⁻¹ d⁻¹
#'   }
#'
#' @return Gas flux in appropriate units:
#'   \itemize{
#'     \item CO₂: kg C ha⁻¹ d⁻¹
#'     \item N₂O: kg N ha⁻¹ d⁻¹
#'     \item CH₄: kg C ha⁻¹ d⁻¹
#'     \item NH₃: kg N ha⁻¹ d⁻¹
#'   }
#'
#' @export
#'
#' @examples
#' # Calculate CO2 flux
#' calculate_gas_flux(
#'   slope = 0.05,  # ppm per second
#'   gas_name = "CO2",
#'   chamber_temp_c = 25,
#'   chamber_height_cm = 30
#' )
#'
#' # Calculate N2O flux
#' calculate_gas_flux(
#'   slope = 0.001,
#'   gas_name = "N2O",
#'   chamber_temp_c = 25,
#'   chamber_height_cm = 30
#' )
#'
calculate_gas_flux <- function(
    slope, gas_name, chamber_temp_c, chamber_height_cm,
    calculation_type = c("Gasmet", "static")) {

  # Validate inputs
  calculation_type <- match.arg(calculation_type)

  if (is.null(slope) || is.null(chamber_temp_c) || is.null(chamber_height_cm)) {
    return(NA_real_)
  }
  if (!is.numeric(slope)) stop("slope must be numeric")
  if (!is.numeric(chamber_temp_c)) stop("chamber_temp_c must be numeric")
  if (!is.numeric(chamber_height_cm) || isTRUE(chamber_height_cm <= 0)) {
    stop("chamber_height_cm must be a positive numeric value")
  }

  # Convert gas_name to standard format
  gas_name <- toupper(gas_name)
  valid_gases <- c("CO2", "N2O", "CH4", "NH3")
  if (!gas_name %in% valid_gases) {
    stop("gas_name must be one of: 'CO2', 'N2O', 'CH4', 'NH3'")
  }

  # Get gas properties
  gas_props <- get_gas_properties(gas_name)

  # Calculate flux
  flux <- slope *
    gas_props$molar_mass *                    # M_m: molar mass (µg µmol⁻¹)
    (1 / (0.0821 * (chamber_temp_c + 273.15))) *  # 1/RT from ideal gas law
    chamber_height_cm *                       # V/A: volume:area ratio (cm)
    (1 / 1000) *                              # Convert cm³ to L
    10000 *                                   # Convert cm² to m²
    3600 *                                    # Convert seconds to hours
    gas_props$element_proportion *            # Proportion of C or N by weight
    0.00024                                   # Convert ug/m2/h to kg/ha/d

  return(flux)
}

#' Get gas-specific properties for flux calculations
#' @noRd
get_gas_properties <- function(gas_name) {
  switch(gas_name,
         "CO2" = list(
           name = "CO2",
           molar_mass = 44.0095,      # g mol⁻¹ = µg µmol⁻¹
           element_proportion = 0.2729, # Proportion C by weight (27.29%)
           element = "C",
           target_unit = "kg C ha⁻¹ d⁻¹"
         ),
         "N2O" = list(
           name = "N2O",
           molar_mass = 44.0128,      # g mol⁻¹ = µg µmol⁻¹
           element_proportion = 0.6365, # Proportion N by weight (63.65%)
           element = "N",
           target_unit = "kg N ha⁻¹ d⁻¹"
         ),
         "CH4" = list(
           name = "CH4",
           molar_mass = 16.04246,     # g mol⁻¹ = µg µmol⁻¹
           element_proportion = 0.7487, # Proportion C by weight (74.87%)
           element = "C",
           target_unit = "kg C ha⁻¹ d⁻¹"
         ),
         "NH3" = list(
           name = "NH3",
           molar_mass = 17.03052,     # g mol⁻¹ = µg µmol⁻¹
           element_proportion = 0.8224, # Proportion N by weight (82.24%)
           element = "N",
           target_unit = "kg N ha⁻¹ d⁻¹"
         ),
         stop("Unknown gas: ", gas_name)
  )
}
