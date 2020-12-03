
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
# Installing from a private repo is requires a Personal Access Token
# https://stackoverflow.com/questions/21171142/how-to-install-r-package-from-private-repo-using-devtools-install-github

remotes::install_git("https://gita.sys.kth.se/kthb/diva", dependencies = TRUE)
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

# Development

Press “Ctrl-Shift-L” to load function, which can then be run, for
example:

``` r
check_invalid_DOI()
# A tibble: 1 x 2
#  DOI                         PID
#  <chr>                     <dbl>
#1 0.1109/ICC.2019.8761470 1498747
```

The cache with downloaded data for publications and author data can be
cleared:

``` r
unlink(
  file.path(rappdirs::app_dir("diva")$config(), 
    c("kth_diva_pubs.rds", "kth_diva_authors.rds")
  )
)
```

## Adding checks

See the file at “`R/checks.R`” for some of the checks. Edit and add
checks there.

Checks using Python can be used too, through `reticulate`, for example.

## Running the API locally

To run the API locally, just open the file (located in
`inst/plumber/checks/plumber.R`) and press play.
