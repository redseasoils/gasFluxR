#' Import Chamber Volume Data
#'
#' @description Imports and cleans up chamber volume Excel spreadsheets
#'   contained in `dir`.
#'
#' @param dir Directory in which to look for chamber volume files. Defaults to
#'   `"data/00_raw/chamber_volume"`.
#' @param file Specific file in `dir` to import. If `NULL` (the default), all
#'   `.xlsx` files in `dir` will be imported.
#' @param file_date_formats Character vector of formats of dates to parse from
#'   `xlsx` file names within `dir`. The default is `%Y%m%d`, which would have a
#'   path like `[dir]/20250425.xlsx`, with the date parsed as `2025-04-25`. If
#'   more than one format is specified, parsing will be attempted in the order
#'   provided. For more information on date parsing abbreviations, see
#'   ?[strptime].
#'
#' @return A data frame of all chamber volume data.
#' @export
#' @importFrom xfun sans_ext
import_chamber_volume <- function(
    dir = 'data/00_raw/chamber_volume',
    file = NULL,
    file_date_formats = c("%Y%m%d")
) {

  if (is.null(file)) {
    cat("\n\nLooking for files in ", getwd(), "/", dir, "\n\n", sep = "")
    # Find files to import
    vol_files <- list.files(
      path = dir,
      recursive = T,
      pattern = "\\.xlsx",
      ignore.case = T,
      full.names = T
    )
  } else {
    vol_files <- file.path(dir, file)
  }

  # Check sheet names
  check_sheets <- function(vol_files, n = 1) {
    sheets <- readxl::excel_sheets(vol_files[n])
    if (length(sheets) > 1 & !"Chamber Volume" %in% sheets) {
      stop(paste("'Chamber Volume' sheet not found in", vol_files[n]))
    }
    if (n < length(vol_files)) check_sheets(vol_files, n + 1)
  }
  check_sheets(vol_files)

  # Load data
  read_sheet <- function(files, n = 1) {
    vol <- list()
    file <- files[n]

    if (length(readxl::excel_sheets(file)) == 1) {
      vol[[file]] <- readxl::read_xlsx(file)
    } else {
      vol[[file]] <- readxl::read_xlsx(file, sheet = "Chamber Volume")
    }

    if (n == length(files)) {
      return(vol)
    } else {
      return(append(vol, read_sheet(files, n = n + 1)))
    }
  }
  vol_dat <- read_sheet(files = vol_files)

  cleanup_chamber_volume <- function(vol_dat, n = 1) {
    vol <- list()
    names(vol_dat[[n]]) <- tolower(names(vol_dat[[n]])) %>% trimws()
    name_changes <- c(
      # New template
      plot = "Plot",
      collar_height_cm = "collar height (cm)",
      sample_in_length_cm = "sample in length (cm)",
      sample_out_length_cm = "sample out length (cm)",
      chamber_temp_c = "chamber temp (c)",
      chamber_temp_c = "chamber temp ( c )",
      soil_moisture_pct = "soil moisture (%)",
      soil_temp_c = "soil temp (c)",
      soil_temp_c = "soil temp ( c )",
      # Old template
      plot = "id",
      collar_height_cm = "collar height",
      sample_in_length_cm = "sample in length",
      sample_out_length_cm = "sample out length",
      collar_height_cm = "collar height in cm2",
      collar_height_cm = "collar height in cm",
      chamber_temp_c = "chamber",
      # Additional
      chamber_temp_c = "chamber temperature (c)",
      soil_temp_c = "soil temperature (c)"
    )
    vol_dat[[n]] <- vol_dat[[n]] %>%
      dplyr::rename(dplyr::any_of(name_changes)) %>%
      dplyr::select(dplyr::any_of(names(name_changes)))

    missing_cols <- names(name_changes)[!names(name_changes) %in% names(vol_dat[[n]])]
    if (length(missing_cols) > 0) {
      missing_cols <- setNames(
        rep(as.numeric(NA), length(missing_cols)),
        missing_cols
      ) %>%
        t() %>%
        as.data.frame()
      vol_dat[[n]] <- dplyr::mutate(vol_dat[[n]], missing_cols)
    }

    vol_dat[[n]] <- vol_dat[[n]] %>%
      dplyr::mutate(
        plot = as.character(plot),
        dplyr::across(c(
          collar_height_cm, sample_in_length_cm, sample_out_length_cm,
          collar_height_cm, chamber_temp_c, soil_moisture_pct,
          soil_temp_c
        ), as.numeric)
      ) %>%
      suppressWarnings()

    vol[[names(vol_dat)[n]]] <- vol_dat[[n]]

    if (n == length(vol_dat)) {
      return(vol)
    } else {
      return(append(vol, cleanup_chamber_volume(vol_dat, n = n + 1)))
    }
  }
  vol_dat <- cleanup_chamber_volume(vol_dat)

  # Create one large data frame from smaller data frames in vol list
  vol <- dplyr::bind_rows(vol_dat, .id = "dir") %>%
    # Add columns for date and site and change class of plot column to factor
    dplyr::mutate(
      Date = lubridate::as_date(xfun::sans_ext(basename(dir)),
                                format = file_date_formats),
      site = stringr::str_split_i(dir, "/", -2) %>% factor(),
      plot = factor(plot), .before = "plot"
    ) %>%
    dplyr::select(-dir) %>%
    dplyr::distinct()

  # Make sure each date/site/plot has only one unique entry of volume data
  multiple_vols <- vol %>% dplyr::filter(dplyr::n() > 1, .by = c(site, Date, plot))
  if (nrow(multiple_vols) > 0) {
    warning(paste(
      "Multiple chamber volume entries for the same plot found at:\n",
      paste(
        unique(paste(
          multiple_vols$site, multiple_vols$Date, sep = " on "
        )),
        collapse = "\n"
      )
    ))
  }

  return(vol)
}

