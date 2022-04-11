
<!-- README.md is generated from README.Rmd. Please edit that file -->

# kthcorpus

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/KTH-Library/kthcorpus/workflows/R-CMD-check/badge.svg)](https://github.com/KTH-Library/kthcorpus/actions)
<!-- badges: end -->

The goal of `kthcorpus` is to provide some tools that can be used when
working with DiVA data.

## Installation

``` r
devtools::install_github("KTH-Library/kthcorpus", dependencies = TRUE)
```

## Example

This is a basic example which shows you how to use some of these tools:

``` r
library(kthcorpus)

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

# Development

Press “Ctrl-Shift-L” to load function, which can then be run, for
example:

``` r
mia <- 
  check_missing_kthid() 

# display results as interactive HTML table
mia %>% 
  DT::datatable(escape = FALSE)
```

The cache with downloaded data for publications and author data can be
refreshed:

``` r
diva_refresh()
```

## Adding checks

See the file at “`R/checks.R`” for some of the checks. Edit and add
checks there.

Checks using Python can be used too, through `reticulate`, for example.

## Running the API locally

To run the API locally, just open the file (located in
`inst/plumber/checks/plumber.R`) and press play.
