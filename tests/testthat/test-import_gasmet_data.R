library(testthat)
library(dplyr)
library(readr)
library(lubridate)

# Helper function to create temporary Gasmet-like files for testing
create_test_gasmet_file <- function(dir, filename, content, date_in_path = "20230115") {
  # Create directory with date in path
  file_dir <- file.path(dir, date_in_path)
  if (!dir.exists(file_dir)) dir.create(file_dir, recursive = TRUE)

  file_path <- file.path(file_dir, filename)
  writeLines(content, file_path)
  return(file_path)
}

test_that("find_gasmet_files finds TXT files correctly", {
  # Create temporary test directory
  temp_dir <- tempfile("gasmet_test_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Create test files
  file.create(file.path(temp_dir, "test1.txt"))
  file.create(file.path(temp_dir, "test2.TXT"))
  file.create(file.path(temp_dir, "test3.csv"))  # Should be ignored
  file.create(file.path(temp_dir, "~$test4.txt")) # Open file, should be ignored if rm_open_files=TRUE

  # Test basic file finding
  files <- find_gasmet_files(dir = temp_dir, recursive = FALSE, rm_open_files = FALSE)
  expect_equal(length(files), 3)
  expect_true(all(grepl("\\.txt$|\\.TXT$", files, ignore.case = TRUE)))

  # Test with rm_open_files = TRUE
  files <- find_gasmet_files(dir = temp_dir, recursive = FALSE, rm_open_files = TRUE)
  expect_equal(length(files), 2)
  expect_false(any(grepl("~\\$", files)))

  # Test error when no files found
  empty_dir <- tempfile("empty_")
  dir.create(empty_dir)
  on.exit(unlink(empty_dir, recursive = TRUE), add = TRUE)

  expect_error(
    find_gasmet_files(dir = empty_dir),
    "No Gasmet TXT files found"
  )
})

test_that("remove_empty_files handles empty files correctly", {
  temp_dir <- tempfile("gasmet_test_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Create empty and non-empty files
  empty_file <- file.path(temp_dir, "empty.txt")
  file.create(empty_file)

  non_empty_file <- file.path(temp_dir, "non_empty.txt")
  writeLines("test data", non_empty_file)

  paths <- c(empty_file, non_empty_file)

  # Should remove empty file with warning
  expect_warning(
    result <- remove_empty_files(paths),
    "Skipping import of empty files"
  )
  expect_equal(length(result), 1)
  expect_equal(result, non_empty_file)
})

test_that("read_gasmet_files reads files correctly", {
  temp_dir <- tempfile("gasmet_test_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Create a test Gasmet file
  test_content <- c(
    "Date\tTime\tCarbon.dioxide.CO2\tNitrous.oxide.N2O",
    "01/15/2023\t12:00:00\t400.5\t0.32",
    "01/15/2023\t12:00:02\t401.2\t0.33",
    "01/15/2023\t12:00:04\t402.1\t0.34"
  )

  file_path <- create_test_gasmet_file(temp_dir, "test_gasmet.txt", test_content)

  # Read files
  files <- find_gasmet_files(dir = temp_dir)
  gasmet_list <- read_gasmet_files(files)

  expect_type(gasmet_list, "list")
  expect_equal(length(gasmet_list), 1)
  expect_true(file_path %in% names(gasmet_list))
  expect_equal(length(gasmet_list[[1]]), 4)  # Header + 3 data rows
})

test_that("remove_repeated_headers removes extra headers correctly", {
  # Create data with repeated headers (common in Gasmet files)
  data_with_repeats <- list(
    "test_file.txt" = c(
      "Date\tTime\tCO2",  # Header
      "01/15/2023\t12:00:00\t400.5",
      "01/15/2023\t12:00:02\t401.2",
      "Date\tTime\tCO2",  # Repeated header
      "01/15/2023\t12:00:04\t402.1",
      "01/15/2023\t12:00:06\t403.3"
    )
  )

  cleaned <- remove_repeated_headers(data_with_repeats)

  expect_equal(length(cleaned[[1]]), 5)  # Header + 4 data rows
  expect_equal(cleaned[[1]][1], "Date\tTime\tCO2")  # First row still header
  expect_false(any(duplicated(cleaned[[1]])))  # No duplicates
})

test_that("import_gasmet_data reads and processes files correctly", {
  temp_dir <- tempfile("gasmet_test_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Create a realistic Gasmet file
  test_content <- c(
    "Date\tTime\tCarbon.dioxide.CO2\tNitrous.oxide.N2O\tMethane.CH4",
    "01/15/2023\t12:00:00\t400.5\t0.32\t1.85",
    "01/15/2023\t12:00:02\t401.2\t0.33\t1.86",
    "01/15/2023\t12:00:04\t402.1\t0.34\t1.87",
    "01/15/2023\t12:00:06\t403.3\t0.35\t1.88"
  )

  file_path <- create_test_gasmet_file(temp_dir, "test_gasmet.txt", test_content, "20230115")

  # Test basic import
  result <- import_gasmet_data(
    dir = temp_dir,
    file_date_formats = "%m/%d/%Y",
    file_time_formats = "%H:%M:%S"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4)
  expect_true("path" %in% names(result))
  expect_true("file" %in% names(result))
  expect_true("seconds" %in% names(result))

  # Check that seconds column increments correctly
  expect_equal(result$seconds, c(0, 2, 4, 6))

  # Check data types
  expect_s3_class(result$Date, "Date")
  expect_s3_class(result$Time, "hms")
  expect_type(result$Carbon.dioxide.CO2, "double")
})

test_that("add_seconds_col calculates seconds correctly", {
  # Test with regular time intervals
  data <- data.frame(
    Time = hms::as_hms(c("12:00:00", "12:00:02", "12:00:04", "12:00:06"))
  )

  result <- add_seconds_col(data, "Time")
  expect_equal(result$seconds, c(0, 2, 4, 6))

  # Test with irregular intervals
  data2 <- data.frame(
    Time = hms::as_hms(c("12:00:00", "12:00:03", "12:00:07", "12:00:10"))
  )

  result2 <- add_seconds_col(data2, "Time")
  expect_equal(result2$seconds, c(0, 3, 7, 10))

  # Test error when time column missing
  data3 <- data.frame(x = 1:3)
  expect_error(
    add_seconds_col(data3, "Time"),
    "Time column not found"
  )
})

test_that("convert_dates_and_times converts formats correctly", {
  data <- data.frame(
    date_char = c("01/15/2023", "01/16/2023"),
    time_char = c("12:00:00", "12:00:02"),
    stringsAsFactors = FALSE
  )

  # Test date conversion
  result <- convert_dates_and_times(
    data,
    date_col = "date_char",
    date_formats = "%m/%d/%Y",
    time_col = NULL,
    time_formats = NULL
  )

  expect_s3_class(result$date_char, "Date")
  expect_equal(as.character(result$date_char), c("2023-01-15", "2023-01-16"))

  # Test time conversion
  result2 <- convert_dates_and_times(
    data,
    date_col = NULL,
    date_formats = NULL,
    time_col = "time_char",
    time_formats = "%H:%M:%S"
  )

  expect_s3_class(result2$time_char, "hms")

  # Test both conversions
  result3 <- convert_dates_and_times(
    data,
    date_col = "date_char",
    date_formats = "%m/%d/%Y",
    time_col = "time_char",
    time_formats = "%H:%M:%S"
  )

  expect_s3_class(result3$date_char, "Date")
  expect_s3_class(result3$time_char, "hms")
})

test_that("check_gasmet_columns identifies missing columns correctly", {
  # Create test data with some missing columns
  data_list <- list(
    "file1.txt" = data.frame(
      Date = "2023-01-15",
      Time = "12:00:00",
      Carbon.dioxide.CO2 = 400.5
      # Missing N2O
    ),
    "file2.txt" = data.frame(
      Date = "2023-01-15",
      Time = "12:00:00",
      Carbon.dioxide.CO2 = 401.2,
      Nitrous.oxide.N2O = 0.33  # Has N2O
    )
  )

  result <- check_gasmet_columns(
    data_list,
    necessary_cols = c("Date", "Time", "Carbon.dioxide.CO2"),
    extra_cols = c("Nitrous.oxide.N2O")
  )

  expect_type(result, "list")
  expect_named(result, c("fatal", "nonfatal"))

  # Should have no fatal errors (all necessary columns present)
  expect_equal(length(result$fatal), 0)

  # Should have nonfatal warning about missing N2O in file1
  expect_true(length(result$nonfatal) > 0)
  expect_match(paste(result$nonfatal, collapse = ""), "Nitrous.oxide.N2O")
  expect_match(paste(result$nonfatal, collapse = ""), "file1.txt")
})

test_that("validate_gasmet_dates identifies date issues correctly", {
  # Create test data with various date scenarios
  data_list <- list(
    # Good file - date matches path
    "path/to/20230115/test1.txt" = data.frame(
      Date = c("01/15/2023", "01/15/2023")
    ),
    # Date mismatch - date doesn't match path
    "path/to/20230115/test2.txt" = data.frame(
      Date = c("01/16/2023", "01/16/2023")
    ),
    # Multiple dates in file
    "path/to/20230115/test3.txt" = data.frame(
      Date = c("01/15/2023", "01/16/2023")
    ),
    # Missing date column (simulated by having wrong column)
    "path/to/20230115/test4.txt" = data.frame(
      WrongDate = c("01/15/2023", "01/15/2023")
    )
  )

  # Need to handle the missing date column case by modifying the list
  names(data_list[[4]]) <- "WrongDate"  # Rename to simulate missing Date column

  result <- validate_gasmet_dates(
    data_list,
    path_n_date = -2,
    file_date_formats = "%m/%d/%Y",
    path_date_formats = "%Y%m%d",
    date_col = "Date"
  )

  expect_type(result, "list")
  expect_named(result, c("date_tests", "message"))

  date_tests <- result$date_tests

  # Check that we have the right number of rows
  expect_gt(nrow(date_tests), 0)

  # Test that messages are generated for issues
  expect_true(!is.null(result$message))

  # Check specific issues
  # File with date mismatch
  mismatch_files <- unique(date_tests$file[date_tests$date_mismatch == TRUE])
  expect_true(any(grepl("test2.txt", mismatch_files)))

  # File with multiple dates
  multi_files <- unique(date_tests$file[date_tests$multiple_dates == TRUE])
  expect_true(any(grepl("test3.txt", multi_files)))

  # File with missing date column
  missing_files <- unique(date_tests$file[date_tests$date_col_missing == TRUE])
  expect_true(any(grepl("test4.txt", missing_files)))
})

test_that("import_gasmet_data handles date test return option", {
  temp_dir <- tempfile("gasmet_test_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Create test file
  test_content <- c(
    "Date\tTime\tCarbon.dioxide.CO2",
    "01/15/2023\t12:00:00\t400.5"
  )

  create_test_gasmet_file(temp_dir, "test.txt", test_content, "20230115")

  # Test return_date_tests = TRUE
  date_tests <- import_gasmet_data(
    dir = temp_dir,
    return_date_tests = TRUE
  )

  expect_s3_class(date_tests, "data.frame")
  expect_true("file" %in% names(date_tests))
  expect_true("date_in_data" %in% names(date_tests))
  expect_true("date_in_file_path" %in% names(date_tests))
})

test_that("import_gasmet_data handles custom column types", {
  temp_dir <- tempfile("gasmet_test_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Create test file with additional columns
  test_content <- c(
    "Date\tTime\tCarbon.dioxide.CO2\tExtraColumn",
    "01/15/2023\t12:00:00\t400.5\textra_data"
  )

  create_test_gasmet_file(temp_dir, "test.txt", test_content, "20230115")

  # Define custom column types
  custom_cols <- readr::cols(
    Date = readr::col_character(),
    Time = readr::col_character(),
    Carbon.dioxide.CO2 = readr::col_double(),
    ExtraColumn = readr::col_character(),
    .default = readr::col_skip()
  )

  result <- import_gasmet_data(
    dir = temp_dir,
    col_types = custom_cols
  )

  expect_true("ExtraColumn" %in% names(result))
  expect_type(result$ExtraColumn, "character")
})

test_that("import_gasmet_data stops on fatal column errors", {
  temp_dir <- tempfile("gasmet_test_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Create test file missing necessary column (Time)
  test_content <- c(
    "Date\tCarbon.dioxide.CO2",  # Missing Time column
    "01/15/2023\t400.5"
  )

  create_test_gasmet_file(temp_dir, "test.txt", test_content, "20230115")

  # Should stop because Time column is necessary
  expect_error(
    import_gasmet_data(
      dir = temp_dir,
      necessary_cols = c("Date", "Time", "Carbon.dioxide.CO2")
    ),
    "Time column not found"
  )
})

test_that("import_gasmet_data handles multiple files correctly", {
  temp_dir <- tempfile("gasmet_test_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Create two test files
  content1 <- c(
    "Date\tTime\tCarbon.dioxide.CO2",
    "01/15/2023\t12:00:00\t400.5",
    "01/15/2023\t12:00:02\t401.2"
  )

  content2 <- c(
    "Date\tTime\tCarbon.dioxide.CO2",
    "01/16/2023\t13:00:00\t402.5",
    "01/16/2023\t13:00:02\t403.2"
  )

  create_test_gasmet_file(temp_dir, "file1.txt", content1, "20230115")
  create_test_gasmet_file(temp_dir, "file2.txt", content2, "20230116")

  result <- import_gasmet_data(
    dir = temp_dir,
    recursive = TRUE,
    file_date_formats = "%m/%d/%Y"
  )

  expect_equal(nrow(result), 4)  # 2 rows from each file
  expect_true("path" %in% names(result))
  expect_true("file" %in% names(result))
  expect_equal(unique(result$file), c("file1", "file2"))
})

test_that("import_gasmet_data handles different date formats", {
  temp_dir <- tempfile("gasmet_test_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Test with ISO date format
  content_iso <- c(
    "Date\tTime\tCarbon.dioxide.CO2",
    "2023-01-15\t12:00:00\t400.5"
  )

  create_test_gasmet_file(temp_dir, "iso.txt", content_iso, "20230115")

  result_iso <- import_gasmet_data(
    dir = temp_dir,
    file_date_formats = c("%Y-%m-%d", "%m/%d/%Y")
  )

  expect_equal(as.character(result_iso$Date[1]), "2023-01-15")
})
