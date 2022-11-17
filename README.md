
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidySALURBAL <img src="man/figures/tidySALURBAL.png" align="right" alt="" width="120" />

<!-- badges: start -->
<!-- badges: end -->

The **tidySALURBAL** package has two goals: 1) to provide utility
functions for the common SALURBAL data cleaning tasks and 2) to allow
users to interface with the SALURBAL data API to return tidyverse-ready
data frames, optionally with feature geometry included.

## Installation

You can install the development version of tidySALURBAL from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Drexel-UHC/tidySALURBAL")
```

## Example

This is a basic example which shows how to use the
`sanitize_codebook_var()` function to convert existing SALURBAL data
identifiers to FAIR variables.

``` r
library(tidySALURBAL)


## Sanatize `APSPM25MEANL1AD`
sanitize_codebook_var('APSPM25MEANL1AD')
```

    ## [1] "APSPM25MEAN"

``` r
## Sanatize `APSPM25MEANL1AD`
sanitize_codebook_var('APSPM25MEANL1AD')
```

    ## [1] "APSPM25MEAN"
