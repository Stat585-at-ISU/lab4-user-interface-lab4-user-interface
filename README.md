
<!-- README.md is generated from README.Rmd. Please edit that file -->

# worldle

<!-- badges: start -->

[![R-CMD-check](https://github.com/Stat585-at-ISU/worldle/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Stat585-at-ISU/worldle/actions/workflows/R-CMD-check.yaml)
[![Coverage
status](https://codecov.io/gh/Stat585-at-ISU/worldle/branch/main/graph/badge.svg)](https://codecov.io/github/Stat585-at-ISU/worldle?branch=main)
<!-- badges: end -->

The goal of worldle is to be able to play a game of guessing a country
based on its shape.

## Installation

You can install the development version of worldle like so:

``` r
remotes::install_github("Stat585-at-ISU/worldle")
```

## Example

How do you play?

``` r
library(worldle)
## basic example code
play_worldle() 
```

You will be shown a shapefile of a country, you have to guess by typing
in the name of a country. See whether you can guess it!

If you prefer, you can play worldle in a shiny app.

``` r
runExample()
```

Get the shapefile of a country:

``` r
url <- "https://geodata.ucdavis.edu/gadm/gadm4.1/shp/gadm41_AUT_shp.zip"
austria <- get_shapes(url)
austria <- thin(austria, 0.001)
```

Now plot the country:

``` r
data(austria)
library(ggplot2)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

austria %>% ggplot() + geom_sf() + theme_void()
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

Would you have guessed that this is Austria?

(We commented out the test in `testthat/test-get_shapes.R` like we did
in class because of the different versions. This helped us pass the R
CMD check GitHub action.)
