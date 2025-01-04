
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{ShinyFOSR}`

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Installation

You can install the development version of `{ShinyFOSR}` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Run

You can launch the application by running:

``` r
ShinyFOSR::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-01-04 01:37:13 CET"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading ShinyFOSR
#> ── R CMD check results ─────────────────────────────── ShinyFOSR 0.0.0.9000 ────
#> Duration: 18s
#> 
#> ❯ checking installed package size ... NOTE
#>     installed size is 29.5Mb
#>     sub-directories of 1Mb or more:
#>       app  29.4Mb
#> 
#> ❯ checking DESCRIPTION meta-information ... NOTE
#>   Malformed Description field: should contain one or more complete sentences.
#> 
#> ❯ checking dependencies in R code ... NOTE
#>   Namespace in Imports field not imported from: ‘refund’
#>     All declared Imports should be used.
#> 
#> ❯ checking R code for possible problems ... NOTE
#>   app_server : <anonymous>: no visible binding for global variable
#>     ‘models’
#>   app_server : <anonymous>: no visible global function definition for
#>     ‘predict’
#>   app_server: no visible binding for global variable ‘y’
#>   app_server: no visible binding for global variable ‘model’
#>   compress_single_model: no visible global function definition for
#>     ‘object.size’
#>   Undefined global functions or variables:
#>     model models object.size predict y
#>   Consider adding
#>     importFrom("stats", "predict")
#>     importFrom("utils", "object.size")
#>   to your NAMESPACE file.
#> 
#> 0 errors ✔ | 0 warnings ✔ | 4 notes ✖
```
