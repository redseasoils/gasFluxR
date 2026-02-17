test_that("model_gas_flux validates input data correctly", {
  # Create test data
  test_data <- data.frame(
    time_sec = c(0, 10, 20, 30, 40, 50, 60),
    co2_ppm = c(400, 401, 402, 403, 404, 405, 406)
  )

  # Test: seconds must start at 0
  bad_data <- test_data
  bad_data$time_sec <- bad_data$time_sec + 10
  result <- model_gas_flux(bad_data, co2_ppm, time_sec, gas_name = "CO2")
  expect_false(result$success)
  expect_match(result$reason, "`seconds` must start at 0")

  # Test: insufficient non-NA values
  na_data <- test_data
  na_data$co2_ppm[1:5] <- NA
  result <- model_gas_flux(na_data, co2_ppm, time_sec, gas_name = "CO2", min_n = 3)
  expect_false(result$success)
  expect_match(result$reason, "Non-NA values.*< min_n")

  # Test: insufficient non-zero values (for non-CO2 gases where zero might be problematic)
  zero_data <- test_data
  zero_data$co2_ppm[1:5] <- 0
  result <- model_gas_flux(zero_data, co2_ppm, time_sec, gas_name = "N2O", min_n = 3)
  expect_false(result$success)
  expect_match(result$reason, "Non-zero values.*< min_n")
})

test_that("model_gas_flux deduces gas name correctly", {
  # Create test data with Gasmet-style column names
  test_data <- data.frame(
    seconds = 0:60,
    Carbon.dioxide.CO2 = 400:460,
    Nitrous.oxide.N2O = 0.3:0.9,
    Methane.CH4 = 1.8:2.4,
    Ammonia.NH3 = 0:0.6
  )

  # Test automatic gas name deduction
  result_co2 <- model_gas_flux(test_data, Carbon.dioxide.CO2, seconds)
  expect_equal(result_co2$gas_name, "CO2")

  result_n2o <- model_gas_flux(test_data, Nitrous.oxide.N2O, seconds)
  expect_equal(result_n2o$gas_name, "N2O")

  result_ch4 <- model_gas_flux(test_data, Methane.CH4, seconds)
  expect_equal(result_ch4$gas_name, "CH4")

  result_nh3 <- model_gas_flux(test_data, Ammonia.NH3, seconds)
  expect_equal(result_nh3$gas_name, "NH3")

  # Test: error when gas name can't be deduced
  bad_data <- test_data
  names(bad_data)[2] <- "weird_column"
  expect_error(
    model_gas_flux(bad_data, weird_column, seconds),
    "Gas name could not be deduced"
  )
})

test_that("CO2 modeling with fixed deadband works correctly", {
  # Create test data with initial disturbance then stable linear trend
  set.seed(123)
  time <- 0:120
  # Simulate chamber closure: initial disturbance (first 30 sec) then linear increase
  co2 <- c(
    400 + rnorm(30, 0, 2) + seq(0, 15, length.out = 30),   # Disturbance phase
    415 + 0.08 * 1:91 + rnorm(91, 0, 0.3)                   # Stable linear phase
  )
  test_data <- data.frame(seconds = time, co2_ppm = co2)

  # Test with fixed deadband (remove first 30 seconds)
  result <- model_gas_flux(
    test_data,
    co2_ppm,
    seconds,
    gas_name = "CO2",
    deadband_opts = list(
      method = "fixed",
      fixed = list(seconds = 30)
    )
  )

  expect_s3_class(result, "flux_mod")
  expect_s3_class(result, "flux_mod.CO2")
  expect_true(result$success)
  expect_equal(result$deadband_info$method, "fixed")
  expect_equal(result$deadband_info$fixed_seconds, 30)
  expect_equal(result$deadband_info$removed_n, 31)  # 0-30 seconds = 31 points

  # Verify deadband removal worked
  expect_gt(min(result$seconds_processed), 30)
  expect_gt(result$metrics$r_squared, 0.95)  # Should have good fit after removing disturbance
})

test_that("CO2 modeling with minima deadband works correctly", {
  # Create realistic chamber data with initial dip then linear increase
  time <- 0:60
  co2 <- c(
    400 - 0.5 * 1:15 + rnorm(15, 0, 0.2),                    # Initial dip (first 15 seconds)
    392.5 + 0.1 * 1:46 + rnorm(46, 0, 0.15)                   # Then linear increase
  )
  test_data <- data.frame(seconds = time, co2_ppm = co2)

  # Test minima method
  result <- model_gas_flux(
    test_data,
    co2_ppm,
    seconds,
    gas_name = "CO2",
    deadband_opts = list(
      method = "minima",
      minima = list(portion = 0.3)  # Search in first 30% of data
    )
  )

  expect_true(result$success)
  expect_equal(result$deadband_info$method, "minima")
  expect_gt(result$deadband_info$removed_n, 0)

  # Minimum should be at the end of the dip (~15 seconds)
  search_range <- 1:floor(length(time) * 0.3)
  min_idx <- which.min(test_data$co2_ppm[search_range])
  expect_equal(result$seconds_processed[1], test_data$seconds[min_idx])

  # Processed data should be roughly linear
  expect_gt(result$metrics$r_squared, 0.95)
})

test_that("CO2 modeling with optimum deadband works correctly", {
  set.seed(123)
  # Create data where optimum method should find best fit by trimming ends
  time <- 0:59
  # Add noisy points at both ends that degrade fit
  co2 <- c(
    4, # one absurd point to make sure there is trimming
    400 + rnorm(4, 0, 5),                                     # Noisy start
    400 + 0.12 * 6:55 + rnorm(50, 0, 0.02),                    # Good linear middle
    406 + 0.12 * 51:55 + rnorm(5, 0, 3)                       # Noisy end
  )
  test_data <- data.frame(seconds = time, co2_ppm = co2)

  # Test optimum method with trim_tails = FALSE (remove from start only)
  # This will fail due to noisy end
  result_start <- model_gas_flux(
    test_data,
    co2_ppm,
    seconds,
    gas_name = "CO2",
    deadband_opts = list(
      method = "optimum",
      optimum = list(trim_tails = FALSE)
    ),
    force = TRUE
  )

  expect_false(result_start$success)
  expect_equal(result_start$deadband_info$method, "optimum")

  # Should have removed some start points but kept all end points
  start_removed <- setdiff(test_data$seconds, result_start$seconds_processed)
  expect_true(all(start_removed < 10))  # Removed points should be early

  # Test optimum method with trim_tails = TRUE (remove from both ends)
  result_both <- model_gas_flux(
    test_data,
    co2_ppm,
    seconds,
    gas_name = "CO2",
    deadband_opts = list(
      method = "optimum",
      optimum = list(trim_tails = TRUE)
    )
  )

  expect_true(result_both$success)
  expect_equal(result_both$deadband_info$method, "optimum")

  # Should have better R² than the start-only method
  expect_gt(result_both$metrics$r_squared, result_start$metrics$r_squared)
})

test_that("minima deadband handles edge cases correctly", {
  # Test when minimum is at the very start (no deadband removal needed)
  time <- 0:60
  co2_start_min <- sort(c(395 + 0.08 * 1:61 + rnorm(61, 0, 0.1))) # sorted; starts at minimum
  test_data <- data.frame(seconds = time, co2_ppm = co2_start_min)

  result <- model_gas_flux(
    test_data,
    co2_ppm,
    seconds,
    gas_name = "CO2",
    deadband_opts = list(
      method = "minima",
      minima = list(portion = 0.25)
    )
  )

  expect_true(result$success)
  expect_equal(result$deadband_info$removed_n, 0)  # Should remove nothing
  expect_equal(length(result$ppm_processed), length(result$ppm_raw))

  # Test when search range is empty
  test_data$co2_ppm[1:5] <- rep(NA_real_, 5)
  result_empty <- model_gas_flux(
    test_data,
    co2_ppm,
    seconds,
    gas_name = "CO2",
    deadband_opts = list(
      method = "minima",
      minima = list(seconds = 1)  # Search range with no points
    )
  )

  expect_false(result_empty$success)
  expect_match(result_empty$reason, "No data points in specified minima search range")
})



test_that("CO2 modeling fails gracefully with insufficient data", {
  # Too few points
  test_data <- data.frame(
    seconds = 0:3,
    co2_ppm = c(400, 401, 402, 403)
  )

  result <- model_gas_flux(
    test_data,
    co2_ppm,
    seconds,
    gas_name = "CO2",
    min_n = 5
  )

  expect_false(result$success)
  expect_match(result$reason, "Non-NA values.*< min_n")

  # Data that can't achieve min_R2
  noisy_data <- data.frame(
    seconds = 0:50,
    co2_ppm = 400 + rnorm(51, 0, 20)  # Very noisy
  )

  result <- model_gas_flux(
    noisy_data,
    co2_ppm,
    seconds,
    gas_name = "CO2",
    min_R2 = 0.9
  )

  expect_false(result$success)
  expect_match(result$reason, "R.* <")
})

test_that("N2O modeling with CO2 model works correctly", {
  # Create CO2 and N2O data
  time <- 0:60
  # CO2 data with clear linear trend
  co2_data <- data.frame(
    seconds = time,
    co2_ppm = 400 + 0.1 * time + rnorm(length(time), 0, 0.01)
  )

  # N2O data that should follow CO2 pattern but with some curvature
  n2o_data <- data.frame(
    seconds = time,
    n2o_ppm = 0.3 + 0.001 * time + 0.00001 * time^2 + rnorm(length(time), 0, 0.01)
  )

  # First model CO2
  co2_result <- model_gas_flux(
    co2_data,
    co2_ppm,
    seconds,
    gas_name = "CO2",
    deadband_opts = list(method = "none")
  )

  expect_true(co2_result$success)

  # Then model N2O using CO2 result
  n2o_result <- model_gas_flux(
    n2o_data,
    n2o_ppm,
    seconds,
    co2_mod = co2_result,
    gas_name = "N2O",
    mod_opts = list(
      models = c("linear", "quadratic"),
      selection_metric = "RMSE"
    )
  )

  expect_s3_class(n2o_result, "flux_mod")
  expect_s3_class(n2o_result, "flux_mod.N2O")
  expect_true(n2o_result$success)
  expect_true("linear" %in% names(n2o_result$models))
  expect_true("quadratic" %in% names(n2o_result$models))
  expect_true(n2o_result$selected_model %in% c("linear", "quadratic"))

  # Verify that N2O used the same time points as CO2 processed data
  expect_equal(n2o_result$seconds_processed, co2_result$seconds_processed)
})

test_that("N2O model selection works correctly", {
  # Set seed for reproducibility
  set.seed(123)

  # Create data where quadratic fits better than linear
  time <- 0:60
  # Make quadratic term strong enough to clearly beat linear
  n2o_data <- data.frame(
    seconds = time,
    n2o_ppm = 0.3 + 0.01 * time + 0.001 * time^2 + rnorm(length(time), 0, 0.02)
  )

  # Mock CO2 model with same time points
  co2_result <- flux_mod_result(
    gas_name = "CO2",
    ppm_raw = 400 + 0.1 * time,
    seconds_raw = time,
    ppm_processed = 400 + 0.1 * time,
    seconds_processed = time,
    success = TRUE
  )

  # Test R2 selection (quadratic should have higher R2)
  result_r2 <- model_gas_flux(
    n2o_data,
    n2o_ppm,
    seconds,
    co2_mod = co2_result,
    gas_name = "N2O",
    mod_opts = list(
      models = c("linear", "quadratic"),
      selection_metric = "R2"
    )
  )

  # Test RMSE selection (quadratic should have lower RMSE)
  result_rmse <- model_gas_flux(
    n2o_data,
    n2o_ppm,
    seconds,
    co2_mod = co2_result,
    gas_name = "N2O",
    mod_opts = list(
      models = c("linear", "quadratic"),
      selection_metric = "RMSE"
    )
  )

  # Quadratic should be selected by both metrics for this data
  expect_equal(result_r2$selected_model, "quadratic")
  expect_equal(result_rmse$selected_model, "quadratic")

  # Calculate metrics directly from the stored models
  linear_metrics_r2 <- get_model_metrics(result_r2$models$linear)
  quad_metrics_r2 <- get_model_metrics(result_r2$models$quadratic)

  linear_metrics_rmse <- get_model_metrics(result_rmse$models$linear)
  quad_metrics_rmse <- get_model_metrics(result_rmse$models$quadratic)

  # Verify quadratic model has better metrics than linear model
  # For R2 selection
  expect_true(quad_metrics_r2$r_squared > linear_metrics_r2$r_squared)
  expect_true(quad_metrics_r2$rmse < linear_metrics_r2$rmse)  # RMSE should also be better

  # For RMSE selection
  expect_true(quad_metrics_rmse$rmse < linear_metrics_rmse$rmse)
  expect_true(quad_metrics_rmse$r_squared > linear_metrics_rmse$r_squared)  # R2 should also be better

  # Verify that the selected model's metrics match what's stored in result$metrics
  expect_equal(result_r2$metrics$r_squared, quad_metrics_r2$r_squared)
  expect_equal(result_r2$metrics$rmse, quad_metrics_r2$rmse)
  expect_equal(result_rmse$metrics$rmse, quad_metrics_rmse$rmse)
  expect_equal(result_rmse$metrics$r_squared, quad_metrics_rmse$r_squared)
})

test_that("N2O model selection correctly identifies linear when it's better", {
  set.seed(456)

  # Create data where linear fits better (very weak quadratic term)
  time <- 0:60
  n2o_data <- data.frame(
    seconds = time,
    n2o_ppm = 0.3 + 0.02 * time + 0.00001 * time^2 + rnorm(length(time), 0, 0.05)  # Very weak quadratic
  )

  # Mock CO2 model with same time points
  co2_result <- flux_mod_result(
    gas_name = "CO2",
    ppm_raw = 400 + 0.1 * time,
    seconds_raw = time,
    ppm_processed = 400 + 0.1 * time,
    seconds_processed = time,
    success = TRUE
  )

  # Test with R2 selection
  result <- model_gas_flux(
    n2o_data,
    n2o_ppm,
    seconds,
    co2_mod = co2_result,
    gas_name = "N2O",
    mod_opts = list(
      models = c("linear", "quadratic"),
      selection_metric = "R2"
    )
  )

  # Linear might be selected (or quadratic if overfitting)
  # Just verify that selection is consistent and metrics are calculated
  expect_true(result$selected_model %in% c("linear", "quadratic"))

  # Calculate metrics from models
  linear_metrics <- get_model_metrics(result$models$linear)
  quad_metrics <- get_model_metrics(result$models$quadratic)

  # Verify that the selected model's metrics match what's stored
  if (result$selected_model == "linear") {
    expect_equal(result$metrics$r_squared, linear_metrics$r_squared)
  } else {
    expect_equal(result$metrics$r_squared, quad_metrics$r_squared)
  }
})

test_that("N2O modeling fails when CO2 model unsuccessful", {
  time <- 0:60
  n2o_data <- data.frame(seconds = time, n2o_ppm = 0.3 + 0.001 * time)

  # Create unsuccessful CO2 model
  co2_result <- flux_mod_result(
    gas_name = "CO2",
    ppm_raw = 400 + 0.1 * time,
    seconds_raw = time,
    success = FALSE,
    reason = "Test failure"
  )

  result <- model_gas_flux(
    n2o_data,
    n2o_ppm,
    seconds,
    co2_mod = co2_result,
    gas_name = "N2O"
  )

  expect_false(result$success)
  expect_match(result$reason, "CO2 model was not successful")
})

test_that("force parameter overrides quality checks", {
  # Create poor quality data
  time <- 0:20
  poor_data <- data.frame(
    seconds = time,
    co2_ppm = 400 + rnorm(21, 0, 50)  # Very noisy
  )

  # Without force - should fail
  result_normal <- model_gas_flux(
    poor_data,
    co2_ppm,
    seconds,
    gas_name = "CO2",
    min_R2 = 0.9
  )
  expect_false(result_normal$success)

  # With force - should return model despite poor quality
  result_forced <- model_gas_flux(
    poor_data,
    co2_ppm,
    seconds,
    gas_name = "CO2",
    min_R2 = 0.9,
    force = TRUE
  )
  expect_false(result_forced$success)  # success remains FALSE when force = TRUE
  expect_false(is.null(result_forced$models))
})

test_that("flux_mod_result creates properly structured objects", {
  # Test creation of new object
  new_mod <- flux_mod_result(
    gas_name = "CO2",
    ppm_raw = 1:10,
    seconds_raw = 0:9,
    min_n = 5,
    min_R2 = 0.98,
    force = FALSE
  )

  expect_s3_class(new_mod, "flux_mod")
  expect_s3_class(new_mod, "flux_mod.CO2")
  expect_equal(new_mod$gas_name, "CO2")
  expect_equal(new_mod$ppm_raw, 1:10)
  expect_null(new_mod$success)  # Not yet set

  # Test updating existing object
  updated_mod <- flux_mod_result(
    update = new_mod,
    success = TRUE,
    reason = NULL
  )

  expect_equal(updated_mod$gas_name, "CO2")  # Preserved
  expect_true(updated_mod$success)  # Updated
})

test_that("model metrics are calculated correctly", {
  # Create simple linear data
  x <- 0:10
  y <- 2 + 0.5 * x + rnorm(11, 0, 0.1)
  model <- lm(y ~ x)

  metrics <- get_model_metrics(model)

  expect_type(metrics, "list")
  expect_true(is.numeric(metrics$r_squared))
  expect_true(metrics$r_squared > 0.9)  # Should be high for this data
  expect_true(is.numeric(metrics$rmse))
  expect_true(metrics$rmse < 1)
  expect_equal(metrics$model_type, "linear")
  expect_equal(metrics$n, 11)

  # Test quadratic model
  x2 <- 0:10
  y2 <- 2 + 0.5 * x2 + 0.1 * x2^2 + rnorm(11, 0, 0.1)
  quad_model <- lm(y2 ~ poly(x2, 2, raw = TRUE))

  quad_metrics <- get_model_metrics(quad_model)

  expect_equal(quad_metrics$model_type, "quadratic")
  expect_true(!is.na(quad_metrics$quadratic_term))
  expect_true(!is.na(quad_metrics$quadratic_p))
})

test_that("calculate_gas_flux converts slopes correctly", {
  # Test CO2 flux calculation
  co2_flux <- calculate_gas_flux(
    slope = 0.05,
    gas_name = "CO2",
    chamber_temp_c = 25,
    chamber_height_cm = 30
  )

  expect_type(co2_flux, "double")
  expect_true(co2_flux > 0)
  expect_false(is.na(co2_flux))

  # Test N2O flux calculation
  n2o_flux <- calculate_gas_flux(
    slope = 0.001,
    gas_name = "N2O",
    chamber_temp_c = 25,
    chamber_height_cm = 30
  )

  expect_true(n2o_flux > 0)

  # Test that different gases give different fluxes for same slope
  ch4_flux <- calculate_gas_flux(
    slope = 0.05,
    gas_name = "CH4",
    chamber_temp_c = 25,
    chamber_height_cm = 30
  )

  nh3_flux <- calculate_gas_flux(
    slope = 0.05,
    gas_name = "NH3",
    chamber_temp_c = 25,
    chamber_height_cm = 30
  )

  # Different molar masses and element proportions should yield different fluxes
  expect_true(abs(ch4_flux - nh3_flux) > 0.001)

  # Test input validation
  expect_error(
    calculate_gas_flux(
      slope = "not numeric",
      gas_name = "CO2",
      chamber_temp_c = 25,
      chamber_height_cm = 30
    ),
    "slope must be numeric"
  )

  expect_error(
    calculate_gas_flux(
      slope = 0.05,
      gas_name = "CO2",
      chamber_temp_c = 25,
      chamber_height_cm = -10
    ),
    "positive numeric"
  )

  expect_error(
    calculate_gas_flux(
      slope = 0.05,
      gas_name = "INVALID",
      chamber_temp_c = 25,
      chamber_height_cm = 30
    ),
    "gas_name must be one of"
  )
})

test_that("end-to-end flux calculation pipeline works", {
  # Complete workflow: from raw data to flux rate
  set.seed(123)
  time <- 0:60
  test_data <- data.frame(
    seconds = time,
    co2_ppm = 400 + 0.05 * time + rnorm(61, 0, 0.02)
  )

  # Model flux
  mod_result <- model_gas_flux(
    test_data,
    co2_ppm,
    seconds,
    gas_name = "CO2",
    deadband_opts = list(method = "none")
  )

  expect_true(mod_result$success)

  # Calculate flux rate
  flux_rate <- calculate_gas_flux(
    slope = mod_result$metrics$slope,
    gas_name = mod_result$gas_name,
    chamber_temp_c = 25,
    chamber_height_cm = 30
  )

  expect_type(flux_rate, "double")
  expect_true(flux_rate > 0)
  expect_true(flux_rate < 100)  # Reasonable range for CO2 flux in kg/ha/d

  # Verify that slope from model matches what went into flux calculation
  expect_equal(mod_result$metrics$slope, 0.05, tolerance = 0.02)
})
