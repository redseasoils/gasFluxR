#' Fit linear model
#' @noRd
fit_linear_model <- function(flux_mod = NULL, x, y) {
  if (is.null(flux_mod)) {
    lm(y ~ x, na.action = na.exclude)
  } else {
    lm(flux_mod$ppm_processed ~ flux_mod$seconds_processed, na.action = na.exclude)
  }
}

#' Fit quadratic model
#' @noRd
fit_quadratic_model <- function(mod) {
  lm(mod$ppm_processed ~ poly(mod$seconds_processed, 2, raw = TRUE),
     na.action = na.exclude)
}

#' Extract model metrics
#' @noRd
get_model_metrics <- function(model) {
  if (is.null(model)) {
    return(list(
      r_squared = NA_real_,
      adj_r_squared = NA_real_,
      rmse = NA_real_,
      aic = NA_real_,
      bic = NA_real_,
      slope = NA_real_,
      intercept = NA_real_,
      quadratic_term = NA_real_,
      slope_p = NA_real_,
      quadratic_p = NA_real_,
      overall_p = NA_real_,
      n = 0,
      sigma = NA_real_,
      model_type = NA_character_
    ))
  }

  summary_mod <- summary(model)
  coefs <- coef(model)
  coef_summary <- summary_mod$coefficients

  # Calculate RMSE
  residuals <- resid(model)
  rmse <- sqrt(mean(residuals^2, na.rm = TRUE))

  # Extract coefficients based on model type
  model_type <- if (length(coefs) == 2) "linear" else "quadratic"

  # Coefficients names and columns
  coef_rnms <- rownames(coef_summary)
  has_intercept <- "(Intercept)" %in% coef_rnms
  coef0 <- if (has_intercept) coef_rnms[1] else NA_character_
  coef1 <- coef_rnms[has_intercept + 1]
  coef2 <- coef_rnms[has_intercept + 2]
  pcol <- "Pr(>|t|)"

  if (model_type == "linear") {
    slope <- unname(coefs[coef1])
    slope_p <- coef_summary[coef1, pcol]
    quadratic_term <- NA_real_
    quadratic_p <- NA_real_
  } else {
    slope <- unname(coefs[coef1])
    quadratic_term <- unname(coefs[coef2])

    # Extract p-values
    slope_p <- coef_summary[coef1, pcol]
    quadratic_p <- coef_summary[coef2, pcol]
  }

  # Overall model F-test p-value
  overall_p <- if (!is.null(summary_mod$fstatistic)) {
    pf(summary_mod$fstatistic[1],
       summary_mod$fstatistic[2],
       summary_mod$fstatistic[3],
       lower.tail = FALSE) %>% unname()
  } else {
    NA_real_
  }

  list(
    r_squared = summary_mod$r.squared,
    adj_r_squared = summary_mod$adj.r.squared,
    rmse = rmse,
    aic = AIC(model),
    bic = BIC(model),
    slope = slope,
    intercept = unname(coefs[coef0]),
    quadratic_term = quadratic_term,
    slope_p = slope_p,
    quadratic_p = quadratic_p,
    overall_p = overall_p,
    n = sum(!is.na(residuals)),
    sigma = summary_mod$sigma,
    model_type = model_type
  )
}

#' Select best model based on specified metric
#' @noRd
select_model <- function(flux_mod, metrics) {
  selection_metric <- flux_mod$mod_opts$selection_metric
  if (!length(selection_metric) == 1) selection_metric <- "RMSE" # Default to RMSE
  if (!selection_metric %in% c("R2", "RMSE")) stop("selection_metric must be one of 'R2' or 'RMSE'")
  R2_vals <- lapply(metrics, \(x) x$r_squared)
  RMSE_vals <- lapply(metrics, \(x) x$rmse)

  if (selection_metric == "R2") { # Higher R2 is better
    winner_idx <- which.max(unlist(R2_vals))
  } else { # Lower RMSE is better
    winner_idx <- which.min(unlist(RMSE_vals))
  }
  winner_nm <- names(metrics)[winner_idx]
  return(winner_nm)
}

#' Create or update flux model result object
#' @noRd
flux_mod_result <- function(
    gas_name = NA_character_,
    ppm_raw = NULL, seconds_raw = NULL,
    ppm_processed = NULL, seconds_processed = NULL,
    min_n = NA_integer_,
    min_R2 = NA_real_,
    success = NULL,
    reason = NULL,
    models = NULL,
    selected_model = NULL,
    metrics = NULL,
    deadband_info = NULL,
    force = NA,
    update = NULL
) {

  if (!is.null(update)) {
    result <- update
    args <- as.list(match.call())[-1]
    args[["update"]] <- NULL
    for (name in names(args)) {
      result[[name]] <- eval(args[[name]], envir = parent.frame())
    }
  } else {
    result <- list(
      gas_name = gas_name,
      ppm_raw = ppm_raw,
      ppm_processed = ppm_processed,
      seconds_raw = seconds_raw,
      seconds_processed = seconds_processed,
      min_n = min_n,
      min_R2 = min_R2,
      success = success,
      reason = reason,
      models = models,
      selected_model = selected_model,
      metrics = metrics,
      deadband_info = deadband_info,
      force = force
    )
  }
  class(result) <- c(paste0('flux_mod.', result$gas_name), "flux_mod", "list")
  return(result)
}

