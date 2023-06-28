
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IMOS Biological Observer <a href='https://github.com/PlanktonTeam/IMOS_BioOceanObserver'><img src='inst/app/www/BOO_Hex.png' style="float:right; height:200px;"></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/PlanktonTeam/IMOS_BioOceanObserver/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PlanktonTeam/IMOS_BioOceanObserver/actions/workflows/R-CMD-check.yaml)
[![issues -
IMOS_BioOceanObserver](https://img.shields.io/github/issues/PlanktonTeam/IMOS_BioOceanObserver)](https://github/PlanktonTeam/IMOS_BioOceanObserver/issues)
<!-- badges: end -->

The goal of the IMOS Biological Observatory is to increase the
availability of IMOS Biological data by allowing users to visualise,
interact and download data products and code.

## Code of Conduct

Please note that the Biological Ocean Observer project is released with
a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## Installation

If you wish to run the Biological Ocean Observer, are a power user, or
would like to contribute to its development, you can install the
development version from [GitHub](https://github.com/) using:

``` r
devtools::install_github("PlanktonTeam/IMOS_BioOceanObserver")
```

and run the application using:

``` r
library(imosboo)
run_app()
```

Please note however, that the above download and installation is not
required if you simply want to use the Biological Ocean Observer. The
latest version will also be available on the [Shiny
Server](%22https://shiny.csiro.au/BioOceanObserver%22)
