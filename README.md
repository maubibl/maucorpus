
<!-- README.md is generated from README.Rmd. Please edit that file -->

# diva

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of `diva` is to provide some tools that can be used when
working with DiVA data.

## Installation

``` r
remotes::install_url("https://gita.sys.kth.se/kthb/diva", dependencies = TRUE)
```

## Example

This is a basic example which shows you how to use some of these tools:

``` r
library(diva)

# download and cache KTH DiVA data
pubs <- kth_diva_pubs()
authors <- kth_diva_authors()
aliases <- kth_diva_aliases()
issues <- kth_diva_issues()

# run some checks
checks <- kth_diva_checks()

# see results
checks
```

## Adding checks

See the file at “`R/checks.R`” for some of the checks. Edit and add
checks there.

Checks using Python can be used too, through `reticulate`, for example.

## Running the API locally

To run the API locally, just open the file (located in
`inst/plumber/checks/plumber.R`) and press play.
