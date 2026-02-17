#' Import data from Gasmet TXT files
#'
#' Import and perform preliminary checks and validations on Gasmet TXT data.
#'
#' @param dir Directory to search for TXT files. By default, search
#'   path is "data/00_raw/gas_concentration". This value can be set globally
#'   using the option "gaseous.gasmet_txt_dir".
#' @param recursive Logical. Should `dir` be searched recursively? Defaults to
#'   `TRUE`.
#' @param rm_open_files Logical. Should open files be removed from the list?
#'   Defaults to `TRUE`.
#' @param col_types List or col_spec object compatible with col_types argument
#'   of [readr::read_tsv()], identifying names and types of columns to be read
#'   in from Gasmet TXT files. If set to `NULL`, a default column specification
#'   will be used which includes Date, Time, and all gas ppm columns.
#' @param necessary_cols Character vector of column names which need to be
#'   present in each Gasmet data frame for data processing to proceed. If all of
#'   these columns are not found in every Gasmet TXT file, an error will be
#'   thrown with information about which files are missing which columns.
#' @param extra_cols Character vector of column names which do not need to be
#'   present in every Gasmet data frame for data processing to proceed, but
#'   which should be checked anyway. Names of files not containing these columns
#'   will be displayed in a message.
#' @param path_n_date Integer. Location of date in path to Gasmet TXT file when
#'   split by the file separator. Defaults to -2, which corresponds to the
#'   directory containing the TXT file. Passed to [stringr::str_split_i()].
#' @param file_date_col Name of date column in Gasmet TXT files. Defaults to
#'   `"Date"`, which should not be changed in most cases.
#' @param file_date_formats Character vector of possible date formats to parse
#'   within TXT file data. Date parsing will be attempted in the order of
#'   formats specified using [lubridate::as_date()]. See [strptime()] for
#'   formatting abbreviations.
#' @param file_time_col Name of time column in Gasmet TXT files. Defaults to
#'   `"Time"`, which should not be changed in most cases.
#' @param file_time_formats String of time format to parse within TXT file data.
#'   Date parsing will be attempted using [readr::parse_time()]. See
#'   [strptime()] for formatting abbreviations. Defaults to `%H:%M:%S`.
#' @param path_date_formats Character vector of possible formats to parse dates
#'   extracted from file paths. Date parsing will be attempted in the order of
#'   formats specified. See [strptime] for formatting abbreviations.
#' @param return_date_tests Logical. Should the date column test results be
#'   returned instead of Gasmet data? Used for debugging only.
#' @param ... Ignored.
#'
#' @returns If `return_date_tests` is FALSE (the default), returns a list of
#'   tibbles (one per gasmet TXT file). Otherwise, returns a data frame of date
#'   test results.
#' @importFrom readr read_tsv cols col_date col_time col_double col_guess
#' @export
#' @md
import_gasmet_data <- function(
    dir = getOption("gaseous.gasmet_txt_dir"),
    recursive = TRUE,
    rm_open_files = TRUE,
    col_types = NULL,
    necessary_cols = c("Date", "Time", "Carbon.dioxide.CO2"),
    extra_cols = c("Nitrous.oxide.N2O", "Methane.CH4", "Ammonia.NH3",
                   "Carbon.monoxide.CO", "Water.vapor.H2O"),
    file_date_col = "Date",
    file_date_formats = c("%m/%d/%Y", "%Y-%m-%d"),
    file_time_col = "Time",
    file_time_formats = c("%H:%M:%S"),
    path_n_date = -2,
    path_date_formats = c("%Y%m%d"),
    return_date_tests = FALSE,
    ...
) {

  gasmet_files <- find_gasmet_files(dir, recursive, rm_open_files)
  gasmet_files <- read_gasmet_files(gasmet_files)
  gasmet_files <- remove_repeated_headers(gasmet_files)

  if (is.null(col_types)) {
    col_types <- readr::cols(
      Date = readr::col_character(),
      Time = readr::col_character(),
      Carbon.dioxide.CO2 = readr::col_double(),
      Nitrous.oxide.N2O = readr::col_double(),
      Methane.CH4 = readr::col_double(),
      Ammonia.NH3 = readr::col_double(),
      Carbon.monoxide.CO = readr::col_double(),
      Water.vapor.H2O = readr::col_double(),
      .default = readr::col_skip()
    )
    cat("Using default column specification to read Gasmet TXT files:\n")
    print(col_types)
    cat("\n")
  }

  data <- lapply(seq_along(gasmet_files), \(x) {
    cat("Reading Gasmet file", paste0(x, ":"),  names(gasmet_files)[x], "\n")
    readr::read_tsv(
      file = I(paste(gasmet_files[[x]], collapse = "\n")),
      col_types = col_types,
      name_repair = "universal_quiet"
    )
  })
  names(data) <- names(gasmet_files)


  # Convert dates and times using specified formats
  data <- lapply(data, \(x) convert_dates_and_times(
    x, file_date_col, file_date_formats, file_time_col, file_time_formats))

  # Add seconds column
  data <- lapply(data, \(x) add_seconds_col(x, file_time_col))

  # Perform checks
  col_check <- check_gasmet_columns(data)
  date_check <- validate_gasmet_dates(data)
  # If return_date_tests is TRUE, return the data from date tests instead of
  # gasmet data
  if (return_date_tests) return(date_check$date_tests)

  # Gather messages
  fatal_msg <- c(date_check$message, col_check$fatal)
  full_msg <- c(fatal_msg, col_check$nonfatal)
  if (!is.null(fatal_msg)) stop(paste(fatal_msg, collapse = "\n"))
  if (!is.null(full_msg)) message(paste(full_msg, collapse = "\n"))

  # Bind data
  data <- dplyr::bind_rows(data, .id = "path")

  # Add filename column
  data <- dplyr::mutate(data, file = xfun::sans_ext(basename(path)), .after = path)

  # Return gasmet data
  return(data)

}

#' Helper to add seconds column to Gasmet TXT data
#' @noRd
add_seconds_col <- function(data, time_col = "Time") {
  if (is.null(time_col) | !time_col %in% names(data)) {
    stop("Time column not found")
  }
  data <- data %>% dplyr::arrange(dplyr::across(tidyselect::all_of(time_col)))
  # time_order <- order(data[[time_col]])
  # data <- data[time_order, ]
  data$seconds <- cumsum(as.integer(diff(c(data[[time_col]][1], data[[time_col]]))))
  return(data)
}

#' Convert date and time columns to specified formats
#'
#' @param data A data frame
#' @param date_col Name of date column in `data`. Set to NULL if no date column
#'   exists.
#' @param date_formats Possible formats of dates in `date_col`. Passed to
#'   [lubridate::as_date()].
#' @param time_col Name of time column in `data`. Set to NULL if no time column
#'   exists.
#' @param time_formats Possible formats of times in `time_col`. Passed to
#'   [readr::parse_time()].
#'
#' @returns A data frame
#' @importFrom lubridate as_date
#' @importFrom readr parse_time
#' @md
convert_dates_and_times <- function(data, date_col = NULL, date_formats,
                                    time_col = NULL, time_formats) {
  if (!is.null(date_col) && date_col %in% names(data))
    data[[date_col]] <- lubridate::as_date(data[[date_col]], format = date_formats)
  if (!is.null(time_col) && time_col %in% names(data))
    data[[time_col]] <- readr::parse_time(data[[time_col]], format = time_formats)
  return(data)
}

#' Find Gasmet TXT files
#'
#' List all Gasmet TXT files in specified directory. By default, the searched
#' directory is `data/00_raw/gas_concentration`.
#'
#' @returns A character vector.
#' @examples
#' if (requireNamespace("gaseous")) {
#'   ex_gasmet_dir <- system.file("example_data/data/00_raw/gas_concentration", package = "gaseous")
#'   find_gasmet_files(ex_gasmet_dir)
#' }
#' @md
find_gasmet_files <- function(
    dir = getOption("gaseous.gasmet_txt_dir"),
    recursive = TRUE,
    rm_open_files = TRUE,
    ...
) {

  files <- list.files(dir, pattern = "\\.[Tt][Xx][Tt]$", recursive = recursive,
                      full.names = TRUE)
  if (rm_open_files) {
    # Remove files beginning with "~$" (actively open files)
    files <- files[!grepl("^~\\$", basename(files))]
  }
  if (length(files) == 0) stop(paste("No Gasmet TXT files found in folder", dir))
  return(files)
}

#' Read Gasmet TXT files as character
#'
#' Find and read Gasmet TXT files as character vectors for preliminary checks.
#'
#' @param paths Character vector. Paths of Gasmet TXT files. By default, the
#'   paths are discovered using the default arguments of [find_gasmet_files()].
#' @param ... Ignored.
#'
#' @returns A list
#' @seealso [find_gasmet_files()]
#' @md
read_gasmet_files <- function(paths = find_gasmet_files(), ...) {
  # Remove empty files before reading in
  read_paths <- remove_empty_files(paths)
  gasmet_files <- suppressWarnings(lapply(read_paths, readLines))
  names(gasmet_files) <- read_paths
  return(gasmet_files)
}


#' Check Gasmet TXT file paths for empty files
#'
#' @param paths A vector of Gasmet TXT file paths identified with
#'   [find_gasmet_files].
#' @param ... Ignored.
#' @returns A vector of paths to empty files.
#' @md
remove_empty_files <- function(paths, ...) {
  info <- lapply(paths, file.info)
  sizes <- lapply(info, \(x) x[["size"]])
  empty <- which(unlist(lapply(sizes, \(x) x == 0)))
  if (length(empty) > 0) {
    warning(paste("Skipping import of empty files:\n", paste(paths[empty], sep = "\n")))
    return(paths[-empty])
  }
  return(paths)
}


#' Remove repeated header rows from Gasmet TXT data
#'
#' @param gasmet_files A list of Gasmet TXT data read using [read_gasmet_files].
#' @param ... Ignored.
#' @returns A list
#' @md
remove_repeated_headers <- function(gasmet_files, ...) {
  lapply(gasmet_files, \(x) {
    extra_headers <- which(x[-1] == x[1]) + 1
    if (length(extra_headers) > 0) return(x[-extra_headers])
    return(x)
  })
}

#' Check column existence in Gasmet TXT files
#'
#' Check for necessary and other expected columns in data from Gasmet TXT files.
#' Compile messages listing files that do not contain specified columns.
#'
#' @param gasmet_data_list List of data frames from Gasmet TXT files, read using
#'   [read_gasmet_files].
#' @param necessary_cols Character vector of column names which need to be
#'   present in each Gasmet data frame for data processing to proceed. Names of
#'   files not containing these columns will be compiled into the returned
#'   "fatal" message.
#' @param extra_cols Character vector of column names which do not need to be
#'   present in every Gasmet data frame for data processing to proceed, but
#'   which should be checked anyway. Names of files not containing these columns
#'   will be compiled into the returned "nonfatal" message.
#' @param ... Ignored.
#'
#' @returns A list of character vectors with names 'fatal' and 'nonfatal'.
#' @md
check_gasmet_columns <- function(
    gasmet_data_list,
    necessary_cols = c("Date", "Time", "Carbon.dioxide.CO2"),
    extra_cols = c("Nitrous.oxide.N2O", "Methane.CH4", "Ammonia.NH3",
                   "Carbon.monoxide.CO", "Water.vapor.H2O"),
    ...
) {
  all_cols <- c(necessary_cols, extra_cols)
  cat('Checking Gasmet data for columns:\n\n', paste(all_cols, collapse = '\n'), '\n')

  file_names <- names(gasmet_data_list)

  # Function to check missing columns for a given file
  check_missing <- function(cols, type = "necessary") {
    purrr::map(cols, function(col) {
      missing_files <- purrr::keep(file_names, function(file) {
        !col %in% names(gasmet_data_list[[file]])
      })

      if (length(missing_files) > 0) {
        paste0(col, " column missing from ", type, " files:\n",
               paste(missing_files, collapse = "\n"), "\n\n")
      }
    }) %>%
      purrr::discard(is.null) %>%
      unlist()
  }

  return(list(
    fatal = check_missing(necessary_cols, "necessary"),
    nonfatal = check_missing(extra_cols, "extra")
  ))
}


#' Validate dates in data from Gasmet TXT files
#'
#' Check that dates in data from Gasmet TXT files (1) are available, (2) are
#' limited to one date per file, (3) match the dates indicated by the file path
#' containing the TXT files, and (4) are formatted in a recognizable way.
#'
#' @param gasmet_data_list List of data frames from Gasmet TXT files, read using
#'   [read_gasmet_files].
#' @param path_n_date Integer. Location of date in path of Gasmet TXT file when
#'   split by the file separator. Defaults to -2, which corresponds to the
#'   directory containing the TXT file. Passed to [stringr::str_split_i()].
#' @param file_date_formats Character vector of possible date formats to parse
#'   within TXT file data. Date parsing will be attempted in the order of
#'   formats specified. See [strptime] for formatting abbreviations.
#' @param path_date_formats Character vector of possible formats to parse dates
#'   extracted from file paths. Date parsing will be attempted in the order of
#'   formats specified. See [strptime] for formatting abbreviations.
#' @param date_col Name of date column in Gasmet files. Defaults to "Date".
#'
#' @returns A list with names "date_tests" (a data frame) and "message" (a
#'   character vector summarizing 'bad' test results).
#' @importFrom stringr str_split_i
#' @importFrom lubridate as_date
#' @md
validate_gasmet_dates <- function(
    gasmet_data_list,
    path_n_date = -2,
    file_date_formats = c("%m/%d/%Y", "%Y-%m-%d"),
    path_date_formats = c("%Y%m%d"),
    date_col = "Date"
) {

  nms <- names(gasmet_data_list)

  tests <- lapply(seq_along(gasmet_data_list), \(x) {
    # Get current TXT file name and path
    cur_file <- nms[x]

    # Get date in TXT file "Date" column
    unique_dates <- unique(gasmet_data_list[[x]][[date_col]])
    date_in_data <- lubridate::as_date(unique_dates, format = file_date_formats)

    # Filter out NAs (failed parses)
    date_in_data <- date_in_data[!is.na(date_in_data)]
    n_dates_in_data <- length(date_in_data)

    # Get date in path of TXT file
    date_in_file_path <- lubridate::as_date(
      stringr::str_split_i(cur_file, .Platform$file.sep, path_n_date),
      format = path_date_formats
    )

    # Check for multiple dates in file
    multiple_dates <- n_dates_in_data > 1

    # Check if date column is missing or all dates failed to parse
    date_col_missing <- n_dates_in_data == 0

    # Check for mismatch of date in file and date in path
    date_mismatch <- if (!multiple_dates && !date_col_missing) {
      # Compare dates - need to handle possible NA in date_in_file_path
      if (is.na(date_in_file_path)) {
        NA  # Can't compare if path date is NA
      } else {
        !any(date_in_data == date_in_file_path, na.rm = TRUE)
      }
    } else {
      NA
    }

    # Return a data frame of dates and test results
    rows <- ifelse(n_dates_in_data == 0, 1, n_dates_in_data)
    result <- data.frame(
      file = as.character(rep(cur_file, rows)),
      date_in_file_path = rep(date_in_file_path, rows),
      date_in_data = if (n_dates_in_data == 0) {
        rep(as.Date(NA), rows)
      } else {
        rep(date_in_data, rows)
      },
      date_col_missing = as.logical(rep(date_col_missing, rows)),
      multiple_dates = as.logical(rep(multiple_dates, rows)),
      date_mismatch = as.logical(rep(date_mismatch, rows))
    )
    return(result)
  })

  tests <- dplyr::bind_rows(tests)

  # Make error message
  msg <- NULL

  date_col_missing <- tests$file[tests$date_col_missing & !duplicated(tests$file)]
  if (length(date_col_missing) > 0) {
    msg <- paste0(
      msg,
      "Date column missing or formatted incorrectly for files:\n",
      paste(date_col_missing, collapse = "\n"),
      "\n\n"
    )
  }

  date_mismatch <- tests$file[tests$date_mismatch & !duplicated(tests$file)]
  if (length(date_mismatch) > 0) {
    msg <- paste0(
      msg,
      "Date in Date column does not match date in file path for files:\n",
      paste(date_mismatch, collapse = "\n"),
      "\n\n"
    )
  }

  multiple_dates <- tests$file[tests$multiple_dates & !duplicated(tests$file)]
  if (length(multiple_dates) > 0) {
    msg <- paste0(
      msg,
      "Multiple dates found in Date column in files:\n",
      paste(multiple_dates, collapse = "\n"),
      "\n\n"
    )
  }

  if (!is.null(msg)) warning(msg)
  return(list(date_tests = tests, message = msg))
}
