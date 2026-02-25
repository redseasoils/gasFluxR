
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gasFluxR

gasFluxR provides a basic workflow processing data from Gasmet portable
gas analyzers, including support for flux modeling and calculations.

## Installation

You can install the development version of gasFluxR from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("redseasoils/gasFluxR")
```

## Getting Started

To calculate gas flux from Gasmet data using the package’s functions,
you’ll need:

1.  A TXT file from a Gasmet portable gas analyzer
2.  A spreadsheet (.xlsx) containing variable chamber dimensions and
    variables, including chamber temperature (°C), collar height (cm; if
    applicable), chamber height (cm), and input and output tube lengths
    (cm).

We recommend storing these files in a specific structure so that the
package’s functions can find many of these files easily and process them
in batches. More on that below, but for now, let’s focus on getting

``` r
library(gasFluxR)
# Use an example TXT file directory
txt_file_dir <- system.file("data", "00_raw", "gas_concentration", "Site1", 
                            "20240830", package = "gasFluxR")
# And a TXT file within that directory
txt_file <- "301 BT.TXT"
# Import
gasmet_data <- import_gasmet_data(txt_file_dir, txt_file)
#> Gasmet file [1] initial read:/Users/ezramoses/Library/Caches/org.R-project.R/R/renv/library/gasFluxR-d5e7aa2e/macos/R-4.5/aarch64-apple-darwin20/gasFluxR/data/00_raw/gas_concentration/Site1/20240830/301 BT.TXT
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
#> Reading Gasmet file 1: /Users/ezramoses/Library/Caches/org.R-project.R/R/renv/library/gasFluxR-d5e7aa2e/macos/R-4.5/aarch64-apple-darwin20/gasFluxR/data/00_raw/gas_concentration/Site1/20240830/301 BT.TXT 
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
#> 1 /Users/ezramoses… 301 … 2024-08-30 09:31:32            3                  605.
#> 2 /Users/ezramoses… 301 … 2024-08-30 09:31:53            2.98               525.
#> 3 /Users/ezramoses… 301 … 2024-08-30 09:32:13            3.03               519.
#> 4 /Users/ezramoses… 301 … 2024-08-30 09:32:34            3.07               529.
#> 5 /Users/ezramoses… 301 … 2024-08-30 09:32:55            3.1                540 
#> 6 /Users/ezramoses… 301 … 2024-08-30 09:33:16            3.13               552.
#> # ℹ 5 more variables: Carbon.monoxide.CO <dbl>, Nitrous.oxide.N2O <dbl>,
#> #   Ammonia.NH3 <dbl>, Methane.CH4 <dbl>, seconds <int>

# Use an example chamber variables spreadsheet
chamber_dir <- system.file("data", "00_raw", "chamber_volume", "Site1", 
                           package = "gasFluxR")
chamber_file <- "20240830.xlsx"
chamber <- import_chamber_volume(chamber_dir, chamber_file)
```

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
root directory containing the “Data” folder, the import functions would
be:

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
