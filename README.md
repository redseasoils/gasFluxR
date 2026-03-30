
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gasFluxR

gasFluxR provides a basic workflow to process raw data from Gasmet
portable gas analyzers. The package supports data processing from import
through to flux calculations. Features include:

1.  Batch import and processing.
2.  Identification and removal of “deadband” (period of measurement
    before equilibrium is reached) using CO<sub>2</sub> data and
    user-specified parameters.

- *In development:* Visualization of CO<sub>2</sub> flux model and
  removed deadband.

3.  Multiple-model fitting (linear and quadratic) and model selection
    using user-specified metric (R<sup>2</sup> or RMSE).

- Currently supported for N<sub>2</sub>O data only.

## Installation

You can install the development version of gasFluxR from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("redseasoils/gasFluxR")
```

## Getting Started

To model gas flux from Gasmet data using the package’s functions, you’ll
need **a TXT file from a Gasmet portable gas analyzer**. To calculate
flux values from the model, you’ll also need the chamber temperature
recorded during the sample, and the chamber height (ratio of chamber
volume to area).

### Data Import

For batch import, we recommend a specific file storage structure. More
on that in [File Storage](#file-storage-for-batch-processing). For now,
let’s import data for a single Gasmet sample (one TXT file) using
\[import_gasmet_data()\].

``` r
library(gasFluxR)
# Use an example TXT file directory
txt_file_dir <- system.file("data", "00_raw", "gas_concentration", "Site1", 
                            "20240830", package = "gasFluxR")
# And a TXT file within that directory
txt_file <- "301 BT.TXT"
# Import
gasmet_data <- import_gasmet_data(txt_file_dir, txt_file)
#> Gasmet file [1] initial read:/Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/library/gasFluxR/data/00_raw/gas_concentration/Site1/20240830/301 BT.TXT
#> Using default column specification to read Gasmet TXT files:
#> cols_only(
#>   Date = col_character(),
#>   Time = col_character(),
#>   Carbon.dioxide.CO2 = col_double(),
#>   Nitrous.oxide.N2O = col_double(),
#>   Methane.CH4 = col_double(),
#>   Ammonia.NH3 = col_double(),
#>   Carbon.monoxide.CO = col_double(),
#>   Water.vapor.H2O = col_double()
#> )
#> 
#> Reading Gasmet file 1: /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/library/gasFluxR/data/00_raw/gas_concentration/Site1/20240830/301 BT.TXT 
#> Checking Gasmet data for columns:
#> 
#>  Date
#> Time
#> Carbon.dioxide.CO2
#> Nitrous.oxide.N2O
#> Methane.CH4
#> Ammonia.NH3
#> Carbon.monoxide.CO
#> Water.vapor.H2O
head(gasmet_data)
#> # A tibble: 6 × 11
#>   path              file  Date       Time     Water.vapor.H2O Carbon.dioxide.CO2
#>   <chr>             <chr> <date>     <time>             <dbl>              <dbl>
#> 1 /Library/Framewo… 301 … 2024-08-30 09:31:32            3                  605.
#> 2 /Library/Framewo… 301 … 2024-08-30 09:31:53            2.98               525.
#> 3 /Library/Framewo… 301 … 2024-08-30 09:32:13            3.03               519.
#> 4 /Library/Framewo… 301 … 2024-08-30 09:32:34            3.07               529.
#> 5 /Library/Framewo… 301 … 2024-08-30 09:32:55            3.1                540 
#> 6 /Library/Framewo… 301 … 2024-08-30 09:33:16            3.13               552.
#> # ℹ 5 more variables: Carbon.monoxide.CO <dbl>, Nitrous.oxide.N2O <dbl>,
#> #   Ammonia.NH3 <dbl>, Methane.CH4 <dbl>, seconds <int>
```

### Model Gas Flux

To model gas flux (ppm sec<sup>-1</sup>), we’ll use
\[model_gas_flux()\]. The function returns a structured list with
information about the data, the model, deadband removal (if applicable),
model selection (if applicable), and model “success.” Model success is
determined by inputs to the function, including:

- `min_n` : the minimum number of observations
- `min_R2` : the minimum R<sup>2</sup> value

We’ll specify a minimum number of **4 observations** and a minimum
R<sup>2</sup> value of **0.98** for a CO<sub>2</sub> model. We’ll also
specify a **fixed** deadband removal method to remove the first **30
seconds** of the measurement.

``` r
flux_mod_CO2 <- model_gas_flux(
  data = gasmet_data,
  ppm_var = Carbon.dioxide.CO2,
  seconds_var = seconds,
  min_n = 4,
  min_R2 = 0.98,
  deadband_opts = list(
    method = "fixed", # specify fixed deadband removal method
    fixed = list(seconds = 30) # specify more options for fixed deadband removal
  )
)
flux_mod_CO2
#> $gas_name
#> [1] "CO2"
#> 
#> $ppm_raw
#>  [1] 605.09 525.02 519.07 528.55 540.00 551.66 565.37 583.04 598.84 612.13
#> [11] 624.83 637.53 649.86
#> 
#> $ppm_processed
#>  [1] 519.07 528.55 540.00 551.66 565.37 583.04 598.84 612.13 624.83 637.53
#> [11] 649.86
#> 
#> $seconds_raw
#>  [1]   0  21  41  62  83 104 125 146 166 187 208 230 251
#> 
#> $seconds_processed
#>  [1]  41  62  83 104 125 146 166 187 208 230 251
#> 
#> $min_n
#> [1] 4
#> 
#> $min_R2
#> [1] 0.98
#> 
#> $success
#> [1] TRUE
#> 
#> $reason
#> NULL
#> 
#> $models
#> $models$linear
#> 
#> Call:
#> lm(formula = flux_mod$ppm_processed ~ flux_mod$seconds_processed, 
#>     na.action = na.exclude)
#> 
#> Coefficients:
#>                (Intercept)  flux_mod$seconds_processed  
#>                   488.0312                      0.6504  
#> 
#> 
#> 
#> $selected_model
#> [1] "linear"
#> 
#> $metrics
#> $metrics$r_squared
#> [1] 0.9964105
#> 
#> $metrics$adj_r_squared
#> [1] 0.9960117
#> 
#> $metrics$rmse
#> [1] 2.585551
#> 
#> $metrics$aic
#> [1] 58.1153
#> 
#> $metrics$bic
#> [1] 59.30898
#> 
#> $metrics$slope
#> [1] 0.6503662
#> 
#> $metrics$intercept
#> [1] 488.0312
#> 
#> $metrics$quadratic_term
#> [1] NA
#> 
#> $metrics$slope_p
#> [1] 2.576697e-12
#> 
#> $metrics$quadratic_p
#> [1] NA
#> 
#> $metrics$overall_p
#> [1] 2.576697e-12
#> 
#> $metrics$n
#> [1] 11
#> 
#> $metrics$sigma
#> [1] 2.858434
#> 
#> $metrics$model_type
#> [1] "linear"
#> 
#> 
#> $deadband_info
#> $deadband_info$method
#> [1] "fixed"
#> 
#> $deadband_info$removed_n
#> [1] 2
#> 
#> $deadband_info$removed_idx
#> [1] 1 2
#> 
#> $deadband_info$fixed_seconds
#> [1] 30
#> 
#> 
#> $force
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "flux_mod.CO2" "flux_mod"     "list"
```

For non-CO<sub>2</sub> gases, we can use the same \[model_gas_flux()\]
function. If we want to apply the CO<sub>2</sub>–identified deadband to
the non-CO<sub>2</sub> model, we can pass the output from the
CO<sub>2</sub> model.

For an N<sub>2</sub>O model, we will also pass model selection options
via the `mod_opts` argument. We will run both linear and quadratic
models on the N<sub>2</sub>O sample, and select the final model using
RMSE. For model quality control, we will specify a minimum of **4
observations** and a **minimum R<sup>2</sup> of 0.1**.

``` r
flux_mod_N2O <- model_gas_flux(
  data = gasmet_data,
  ppm_var = Nitrous.oxide.N2O,
  seconds_var = seconds,
  co2_mod = flux_mod_CO2,
  min_n = 4,
  min_R2 = 0.1,
  mod_opts = list(
    models = c("linear", "quadratic"),  # Models to fit
    selection_metric = "RMSE"  # Metric for best model selection
  )
)
flux_mod_N2O
#> $gas_name
#> [1] "N2O"
#> 
#> $ppm_raw
#>  [1] 0.4122 0.3977 0.3984 0.3863 0.3859 0.3912 0.3898 0.3909 0.3817 0.3906
#> [11] 0.3877 0.3854 0.3873
#> 
#> $ppm_processed
#>  [1] 0.3984 0.3863 0.3859 0.3912 0.3898 0.3909 0.3817 0.3906 0.3877 0.3854
#> [11] 0.3873
#> 
#> $seconds_raw
#>  [1]   0  21  41  62  83 104 125 146 166 187 208 230 251
#> 
#> $seconds_processed
#>  [1]  41  62  83 104 125 146 166 187 208 230 251
#> 
#> $min_n
#> [1] 4
#> 
#> $min_R2
#> [1] 0.1
#> 
#> $success
#> [1] TRUE
#> 
#> $reason
#> NULL
#> 
#> $models
#> $models$linear
#> 
#> Call:
#> lm(formula = flux_mod$ppm_processed ~ flux_mod$seconds_processed, 
#>     na.action = na.exclude)
#> 
#> Coefficients:
#>                (Intercept)  flux_mod$seconds_processed  
#>                  3.926e-01                  -2.729e-05  
#> 
#> 
#> $models$quadratic
#> 
#> Call:
#> lm(formula = mod$ppm_processed ~ poly(mod$seconds_processed, 
#>     2, raw = TRUE), na.action = na.exclude)
#> 
#> Coefficients:
#>                                 (Intercept)  
#>                                   3.970e-01  
#> poly(mod$seconds_processed, 2, raw = TRUE)1  
#>                                  -1.033e-04  
#> poly(mod$seconds_processed, 2, raw = TRUE)2  
#>                                   2.604e-07  
#> 
#> 
#> 
#> $selected_model
#> [1] "quadratic"
#> 
#> $metrics
#> $metrics$r_squared
#> [1] 0.2537954
#> 
#> $metrics$adj_r_squared
#> [1] 0.06724427
#> 
#> $metrics$rmse
#> [1] 0.003556209
#> 
#> $metrics$aic
#> [1] -84.84267
#> 
#> $metrics$bic
#> [1] -83.25109
#> 
#> $metrics$slope
#> [1] -0.0001032892
#> 
#> $metrics$intercept
#> [1] 0.3970351
#> 
#> $metrics$quadratic_term
#> [1] 2.603621e-07
#> 
#> $metrics$slope_p
#> [1] 0.3127477
#> 
#> $metrics$quadratic_p
#> [1] 0.4420788
#> 
#> $metrics$overall_p
#> [1] 0.3100499
#> 
#> $metrics$n
#> [1] 11
#> 
#> $metrics$sigma
#> [1] 0.004170025
#> 
#> $metrics$model_type
#> [1] "quadratic"
#> 
#> 
#> $deadband_info
#> NULL
#> 
#> $force
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "flux_mod.N2O" "flux_mod"     "list"
```

For NH<sub>3</sub> and CH<sub>4</sub> models, we can again pass the
CO<sub>2</sub> model output to remove the CO<sub>2</sub> identified
deadband from the sample before modeling, and set our minimum number of
observations and R<sup>2</sup>. We do not need to pass any other
species-specific parameters.

``` r
flux_mod_CH4 <- model_gas_flux(
  data = gasmet_data,
  ppm_var = Methane.CH4,
  seconds_var = seconds,
  co2_mod = flux_mod_CO2,
  min_n = 4,
  min_R2 = 0.1
)
flux_mod_CH4
#> $gas_name
#> [1] "CH4"
#> 
#> $ppm_raw
#>  [1] 2.150 2.069 2.137 2.152 2.156 2.134 2.102 2.100 2.139 2.093 2.110 2.152
#> [13] 2.116
#> 
#> $ppm_processed
#>  [1] 2.137 2.152 2.156 2.134 2.102 2.100 2.139 2.093 2.110 2.152 2.116
#> 
#> $seconds_raw
#>  [1]   0  21  41  62  83 104 125 146 166 187 208 230 251
#> 
#> $seconds_processed
#>  [1]  41  62  83 104 125 146 166 187 208 230 251
#> 
#> $min_n
#> [1] 4
#> 
#> $min_R2
#> [1] 0.1
#> 
#> $success
#> [1] TRUE
#> 
#> $reason
#> NULL
#> 
#> $models
#> $models$linear
#> 
#> Call:
#> lm(formula = flux_mod$ppm_processed ~ flux_mod$seconds_processed, 
#>     na.action = na.exclude)
#> 
#> Coefficients:
#>                (Intercept)  flux_mod$seconds_processed  
#>                  2.1446044                  -0.0001245  
#> 
#> 
#> 
#> $selected_model
#> [1] "linear"
#> 
#> $metrics
#> $metrics$r_squared
#> [1] 0.1412266
#> 
#> $metrics$adj_r_squared
#> [1] 0.04580733
#> 
#> $metrics$rmse
#> [1] 0.0203428
#> 
#> $metrics$aic
#> [1] -48.47398
#> 
#> $metrics$bic
#> [1] -47.28029
#> 
#> $metrics$slope
#> [1] -0.0001245465
#> 
#> $metrics$intercept
#> [1] 2.144604
#> 
#> $metrics$quadratic_term
#> [1] NA
#> 
#> $metrics$slope_p
#> [1] 0.2547056
#> 
#> $metrics$quadratic_p
#> [1] NA
#> 
#> $metrics$overall_p
#> [1] 0.2547056
#> 
#> $metrics$n
#> [1] 11
#> 
#> $metrics$sigma
#> [1] 0.02248981
#> 
#> $metrics$model_type
#> [1] "linear"
#> 
#> 
#> $deadband_info
#> NULL
#> 
#> $force
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "flux_mod.CH4" "flux_mod"     "list"
```

``` r
flux_mod_NH3 <- model_gas_flux(
  data = gasmet_data,
  ppm_var = Ammonia.NH3,
  seconds_var = seconds,
  co2_mod = flux_mod_CO2,
  min_n = 4,
  min_R2 = 0.1
)
flux_mod_NH3
#> $gas_name
#> [1] "NH3"
#> 
#> $ppm_raw
#>  [1] 0.000 0.000 0.000 0.022 0.000 0.009 0.001 0.000 0.000 0.012 0.018 0.000
#> [13] 0.000
#> 
#> $ppm_processed
#>  [1] 0.000 0.022 0.000 0.009 0.001 0.000 0.000 0.012 0.018 0.000 0.000
#> 
#> $seconds_raw
#>  [1]   0  21  41  62  83 104 125 146 166 187 208 230 251
#> 
#> $seconds_processed
#>  [1]  41  62  83 104 125 146 166 187 208 230 251
#> 
#> $min_n
#> [1] 4
#> 
#> $min_R2
#> [1] 0.1
#> 
#> $success
#> [1] FALSE
#> 
#> $reason
#> [1] "R² (0.012) < 0.1"
#> 
#> $models
#> NULL
#> 
#> $selected_model
#> NULL
#> 
#> $metrics
#> NULL
#> 
#> $deadband_info
#> NULL
#> 
#> $force
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "flux_mod.NH3" "flux_mod"     "list"
```

Note that the NH<sub>3</sub> flux model fails to meet our quality
control parameters, so the output flags `"success"` as `FALSE` and adds
a `"reason"` for model failure. Also note that `"models"` and
`"metrics"` are both `NULL` as the function does not by default return
models and metrics when quality control parameters are not met. To
overcome this, we can tell the function to return the model regardless
of quality control status via the `force` argument.

``` r
flux_mod_NH3_forced <-  model_gas_flux(
  data = gasmet_data,
  ppm_var = Ammonia.NH3,
  seconds_var = seconds,
  co2_mod = flux_mod_CO2,
  min_n = 4,
  min_R2 = 0.1,
  force = TRUE
)
flux_mod_NH3_forced
#> $gas_name
#> [1] "NH3"
#> 
#> $ppm_raw
#>  [1] 0.000 0.000 0.000 0.022 0.000 0.009 0.001 0.000 0.000 0.012 0.018 0.000
#> [13] 0.000
#> 
#> $ppm_processed
#>  [1] 0.000 0.022 0.000 0.009 0.001 0.000 0.000 0.012 0.018 0.000 0.000
#> 
#> $seconds_raw
#>  [1]   0  21  41  62  83 104 125 146 166 187 208 230 251
#> 
#> $seconds_processed
#>  [1]  41  62  83 104 125 146 166 187 208 230 251
#> 
#> $min_n
#> [1] 4
#> 
#> $min_R2
#> [1] 0.1
#> 
#> $success
#> [1] FALSE
#> 
#> $reason
#> [1] "R² (0.012) < 0.1"
#> 
#> $models
#> $models$linear
#> 
#> Call:
#> lm(formula = flux_mod$ppm_processed ~ flux_mod$seconds_processed, 
#>     na.action = na.exclude)
#> 
#> Coefficients:
#>                (Intercept)  flux_mod$seconds_processed  
#>                  7.515e-03                  -1.289e-05  
#> 
#> 
#> 
#> $selected_model
#> [1] "linear"
#> 
#> $metrics
#> $metrics$r_squared
#> [1] 0.0117143
#> 
#> $metrics$adj_r_squared
#> [1] -0.09809522
#> 
#> $metrics$rmse
#> [1] 0.007842347
#> 
#> $metrics$aic
#> [1] -69.44413
#> 
#> $metrics$bic
#> [1] -68.25044
#> 
#> $metrics$slope
#> [1] -1.289036e-05
#> 
#> $metrics$intercept
#> [1] 0.00751484
#> 
#> $metrics$quadratic_term
#> [1] NA
#> 
#> $metrics$slope_p
#> [1] 0.7514255
#> 
#> $metrics$quadratic_p
#> [1] NA
#> 
#> $metrics$overall_p
#> [1] 0.7514255
#> 
#> $metrics$n
#> [1] 11
#> 
#> $metrics$sigma
#> [1] 0.008670041
#> 
#> $metrics$model_type
#> [1] "linear"
#> 
#> 
#> $deadband_info
#> NULL
#> 
#> $force
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "flux_mod.NH3" "flux_mod"     "list"
```

When we set `force` to `TRUE`, we will get the model and its metrics in
the output, even if the model fails quality control.

<!-- ### Calculate Gas Flux -->

<!-- 2. A spreadsheet (.xlsx) containing variable chamber dimensions and variables, including chamber temperature (°C), collar height (cm; if applicable), chamber height (cm), and input and output tube lengths (cm). -->

<!-- ```{r import-volume} -->

<!-- # Use an example chamber variables spreadsheet -->

<!-- chamber_dir <- system.file("data", "00_raw", "chamber_volume", "Site1",  -->

<!--                            package = "gasFluxR") -->

<!-- chamber_file <- "20240830.xlsx" -->

<!-- chamber <- import_chamber_volume(chamber_dir, chamber_file) -->

<!-- head(chamber) -->

<!-- ``` -->

## File Storage for Batch Processing

Files must be structured in a standard format to batch process Gasmet
data. File structure must meet the following conditions:

1.  All Gasmet TXT files must be stored in a directory in which there
    are no other TXT files. The files *can* be stored in subdirectories,
    but there must be one main directory below which the only TXT files
    are Gasmet files.
2.  Similarly, all spreadsheets containing chamber variables must be
    stored in a directory which contains no other .xlsx files (this can
    be the same as the TXT file directory). These files may also be
    stored in subdirectories.

Below is an example file structure, in which “Gasmet_Data/raw” is the
main directory for both Gasmet TXT files and spreadsheets:

``` bash
└──Gasmet_Data
│   └── raw
│   │   ├── Site_A
│   │   │   ├── 20240701
│   │   │   │   ├── 1101.TXT
│   │   │   │   ├── 1102.TXT
│   │   │   │   ├── 1201.TXT
│   │   │   │   ├── 1202.TXT
│   │   │   │   └── Chamber.xlsx
│   │   │   ├── 20240708
│   │   │   │   ├── 1101.TXT
│   │   │   │   ├── 1102.TXT
│   │   │   │   ├── 1201.TXT
│   │   │   │   ├── 1202.TXT
│   │   │   │   └── Chamber.xlsx
│   │   └── Site_B
│   │   │   ├── 20240702
│   │   │   │   ├── 2101.TXT
│   │   │   │   ├── 2102.TXT
│   │   │   │   ├── 2201.TXT
│   │   │   │   ├── 2202.TXT
│   │   │   │   └── Chamber.xlsx
│   │   │   ├── 20240709
│   │   │   │   ├── 2101.TXT
│   │   │   │   ├── 2102.TXT
│   │   │   │   ├── 2201.TXT
│   │   │   │   ├── 2202.TXT
│   │   │   │   └── Chamber.xlsx
```

With the file structure above, assuming the working directory is the
root directory containing the `Gasmet_Data` folder, the import functions
would be:

``` r
# Set option for TXT file directory, then import
options("gaseous.gasemt_TXT_dir" = "Data/GHG/Gasmet/raw")
import_gasmet_data()

# OR specify the path in the import function, without option setting
import_gasmet_data("Data/GHG/Gasmet/raw")

# Chamber measurements
import_chamber_volume("Data/GHG/Gasmet/raw")
```

## Flux Modeling Procedures

The function `model_gas_flux()` detects the gas species via the column
name in the `ppm_var` argument (or using `gas_name` directly, if
specified). The data are dispatched to modeling procedures based on
species:

- **CO<sub>2</sub>**: Linear model with four options for deadband
  (i.e. initial chamber disturbance) removal:
  1.  None: All data are modeled
  2.  Fixed: Remove inital observations up to a certain number of
      seconds into the measurement (typically 30)
  3.  Minima: Remove initial observations up to a local minima
  4.  Optimum: Remove initial observations until an R<sup>2</sup>
      threshold is achieved (success) or too few observations remain
      (failure).
- **N<sub>2</sub>O**: Linear and quadratic models with model selection
  metric specified by user (RMSE or R<sup>2</sup>). Results from
  CO<sub>2</sub> modeling can be passed to remove
  CO<sub>2</sub>-identified deadband from N<sub>2</sub>O models and/or
  fail N<sub>2</sub>O models if the CO<sub>2</sub> model failed.
- **Other**: Linear modeling with the option to pass results from
  CO<sub>2</sub> modeling to remove CO<sub>2</sub>-identified deadband
  and/or fail if the CO<sub>2</sub> model failed.
