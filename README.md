
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{ShinyFOSR}` <a href='https://github.com/EmanuelSommer/ShinyFOSR'><img src='inst/app/www/university_of_essex_logo.gif' align="right" width="20%" /></a>

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

## About

You are reading the doc about version: 0.0.1

`{ShinyFOSR}` is a Shiny app that allows users to interactively explore
the functional regression models presented in the paper *Predicting
normative walking biomechanics across the lifespan using seven simple
features* by Liew et al. (2025).

A [**free online version**](https://esommer.shinyapps.io/ShinyFOSR/) is
available for testing.

> **Note**: The online version is limited in functionality and does not
> support Confidence Intervals (CI). Also, it is has limited monthly
> usage and may be unavailable at times. If you are interested in the
> full and fast version, please follow the installation instructions
> below.

## Installation

### Fully Fledged Version

For the full version that also supports Confidence Intervals (CI) you
need to download the file containing the models with enhanced
uncertainty quantification capabilities from [this permanent
link](train_mod_sparse_sd.RDS). Follow the instructions below to install
the package and run the app.

1.  Clone the repository to your local machine and navigate to the
    directory in your terminal.

``` bash
git clone
cd ShinyFOSR
```

2.  Place the downloaded file `train_mod_sparse_sd.RDS` into the
    `inst/app/www/` directory.
3.  Install the dependencies using `renv`.

``` r
renv::restore()
```

4.  Run the app by running the whole `dev/run_dev.R` script in your R
    console.

``` r
source("dev/run_dev.R")
```

### Package Version (Limited Functionality)

You can install the development version of `{ShinyFOSR}` like so:

``` r
devtools::install_github("EmanuelSommer/ShinyFOSR")
```

Then to launch the application by running the following code in your R
console:

``` r
ShinyFOSR::run_app()
```

> Any failure of the app is then likely connected to missing
> (potentially large) data files.

------------------------------------------------------------------------

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-03-07 16:50:09 CET"
```
