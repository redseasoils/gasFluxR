# === === === === === === === === === === === === === === === === === === === ==
# GASMET DATA FLUX CALCULATIONS ----
# === === === === === === === === === === === === === === === === === === === ==

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# IMPORT GASMET TXT DATA ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --

# If needed, change the file path in the `options` call below to the directory
# containing Gasmet TXT files.
# options("gaseous.gasmet_txt_dir" = "data/GHG/00_raw/gas_concentration/")

gasmet_data <- import_gasmet_data()
head(gasmet_data)

# Define experimental unit ("EU") and sampling unit ("SU") in new columns. EU is
# typically the plot ID (e.g., "101", "102", etc.). SU is typically the plot ID
# or the plot ID + sampling location (e.g., "101 BT") or technical replicate
# (e.g., "101-1", "101-2", "101 BT-1").
#
# Flux models will be constructed for each SU/Date. Therefore, if you have
# multiple sites with the same plot IDs, these need to be modified in the SU
# column to be unique across sites (e.g., if sites 1 and 2 both have a plot
# "101", make the SU "S1-101" for site 1 and "S2-101" for site 2).
#
# In the code chunk below, the SU and EU are both set as the name of the first
# subdirectory under "gaseous.gasmet_txt_dir" (usually the site ID) pasted with
# the TXT file name (e.g. for a TXT file at path
# "data/GHG/00_raw/gas_concentration/site1/20251011/101.TXT", the new column
# entry would be "site1__101"). This code may need to be modified to the
# specific project's needs.
gasmet_data <- gasmet_data %>%
  dplyr::mutate(
    EU = paste0(stringr::str_split_i(path, .Platform$file.sep, -3), "__", file),
    SU = paste0(stringr::str_split_i(path, .Platform$file.sep, -3), "__", file),
  )

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# IMPORT CHAMBER VOLUME AND TEMPERATURE DATA ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --

chamber_vol <- import_chamber_volume("../gaseous/inst/example_data/data/00_raw/chamber_volume")
head(chamber_vol)

# Same as above - create EU and SU columns. These should match the EU and SU
# columns in gasmet_data, so that they can be used as lookup columns to join the
# data later on.
chamber_vol <- chamber_vol %>%
  dplyr::mutate(
    EU = paste0(site, "__", plot),
    SU = paste0(site, "__", plot)
  )

# Define grouping variables for modeling. Should always be "Date" and "SU", if
# instructions above were followed.
grp <- c("Date", "SU")

# Fill in missing collar heights
chamber_vol <- chamber_vol %>%
  dplyr::arrange(dplyr::across(tidyselect::all_of(grp))) %>%
  dplyr::group_by(dplyr::across(tidyselect::all_of(grp))) %>%
  tidyr::fill(collar_height_cm, .direction = "downup") %>%
  dplyr::ungroup()

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# CONSTRUCT FLUX MODELS ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --

# Nest data by group
flux_mods <- gasmet_data %>%
  dplyr::nest_by(dplyr::across(tidyselect::all_of(grp)), .key = "gasmet_data")

flux_mods <- flux_mods %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    CO2 = model_gas_flux(
      gasmet_data, Carbon.dioxide.CO2, seconds, min_n = 4, min_R2 = 0.98,
      deadband_opts = list(method = "optimum",
                           optimum = list(trim_tails = FALSE))) %>% list(),
    N2O = model_gas_flux(
      gasmet_data, Nitrous.oxide.N2O, seconds, co2_mod = CO2, min_n = 4,
      min_R2 = 0.1, model_opts = list(models = c("linear", "quadratic"),
                                      selection_metric = "RMSE")) %>% list(),
    NH3 = model_gas_flux(
      gasmet_data, Ammonia.NH3, seconds, co2_mod = CO2, min_n = 4,
      min_R2 = 0.1) %>% list(),
    CH4 = model_gas_flux(
      gasmet_data, Methane.CH4, seconds, co2_mod = CO2, min_n = 4,
      min_R2 = 0.1) %>% list()
  ) %>%
  dplyr::ungroup()

# Join with chamber volume / temp measurements
ghg <- flux_mods %>% dplyr::full_join(chamber_vol, by = grp)
head(ghg)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# CALCULATE FLUX ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --

# * 1. CHAMBER HEIGHT ----
ghg <- ghg %>%
  dplyr::mutate(

    # Calculate collar volume
    collar_radius_cm     = 10.15,
    collar_area_cm2      = collar_radius_cm ^ 2 * pi,
    collar_vol_cm3       = collar_area_cm2 * collar_height_cm,

    # Calculate volume of tubes leading in and out of chamber
    sample_tube_radius   = 0.2159,
    sample_in_vol_cm3    = pi * sample_tube_radius ^ 2 * sample_in_length_cm,
    sample_out_vol_cm3   = pi * sample_tube_radius ^ 2 * sample_out_length_cm,

    # Define unit volume for Gasmet (standard)
    unit_vol_cm3         = 500,

    # Measured chamber height (not volume to area ratio)
    chamber_height_measured_cm = 15,
    chamber_vol_cm3      = collar_area_cm2 * chamber_height_measured_cm,
    total_vol_cm3        = collar_vol_cm3 + sample_in_vol_cm3 +
                             sample_out_vol_cm3 + chamber_vol_cm3 + unit_vol_cm3,
    chamber_height_cm    = total_vol_cm3 / collar_area_cm2,

    # Calculate chamber height (cm) as the ratio of chamber volume (cm^3) to
    # chamber area (cm^3).
    chamber_height_cm = total_vol_cm3 / collar_area_cm2
  )

# 2. FLUX ----
flux <- ghg %>%
  dplyr::rowwise() %>%
  dplyr::mutate(

    CO2 = calculate_gas_flux(
      slope = CO2$metrics$slope,
      gas_name = "CO2",
      chamber_height_cm = chamber_height_cm,
      chamber_temp_c = chamber_temp_c,
      calculation_type = "Gasmet"
    ) %>% units::set_units(kg/hectare/day),

    N2O = calculate_gas_flux(
      slope = N2O$metrics$slope,
      gas_name = "N2O",
      chamber_height_cm = chamber_height_cm,
      chamber_temp_c = chamber_temp_c,
      calculation_type = "Gasmet"
    ) %>% units::set_units(kg/hectare/day),

    NH3 = calculate_gas_flux(
      slope = NH3$metrics$slope,
      gas_name = "NH3",
      chamber_height_cm = chamber_height_cm,
      chamber_temp_c = chamber_temp_c,
      calculation_type = "Gasmet"
    ) %>% units::set_units(kg/hectare/day),

    CH4 = calculate_gas_flux(
      slope = CH4$metrics$slope,
      gas_name = "CH4",
      chamber_height_cm = chamber_height_cm,
      chamber_temp_c = chamber_temp_c,
      calculation_type = "Gasmet"
    ) %>% units::set_units(kg/hectare/day)

  ) %>%
  dplyr::ungroup()
summary(dplyr::select(flux, tidyselect::any_of(c(
  'collar_height_cm', 'chamber_temp_c', 'CO2', 'N2O', 'NH3', 'CH4'))))
