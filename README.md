
# incidence2

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/incidence2)](https://CRAN.R-project.org/package=incidence2)
[![R build
status](https://github.com/reconhub/incidence2/workflows/R-CMD-check/badge.svg)](https://github.com/reconhub/incidence2/actions)
[![codecov](https://codecov.io/gh/reconhub/incidence2/branch/master/graph/badge.svg)](https://codecov.io/gh/reconhub/incidence2)
<!-- badges: end -->

**Disclaimer:** this package is a work in progress, in constant
development. Please reach out to the authors before using.

*incidence2* refocusses the scope of the original
[incidence](https://github.com/reconhub/incidence) package. The aim is
to provide a “tidy” interface for users to work with whilst at the same
time simplifying the underlying implementation.

Whilst we are in the early stages of development, we aim to re-implement
the “fitting” functions of
[incidence](https://github.com/reconhub/incidence)
(e.g. `incidence::fit`) in a smaller, separate, package with a more
consistent interface, more choice of underlying models, and tidier
outputs. To this end, the package scope of *incidence2* focusses only on
the initial data handling, calculation and graphing of incidence
objects.

## Installing the package

To install the development, *github* version of the package use:

``` r
devtools::install_github("reconhub/incidence")
```
