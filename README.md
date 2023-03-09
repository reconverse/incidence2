
<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/incidence2)](https://CRAN.R-project.org/package=incidence2)
[![Codecov test
coverage](https://codecov.io/gh/reconverse/incidence2/branch/master/graph/badge.svg)](https://codecov.io/gh/reconverse/incidence2?branch=master)
[![R-CMD-check](https://github.com/reconverse/incidence2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/reconverse/incidence2/actions/workflows/R-CMD-check.yaml)
[![](https://raw.githubusercontent.com/reconverse/reconverse.github.io/master/images/badge-stable.svg)](https://www.reconverse.org/lifecycle.html#stable)
<!-- badges: end -->

# incidence2

{incidence2} is an R package that implements functions and classes to
compute, handle and visualise *incidence* from linelist data. It
refocusses the scope of the original
[{incidence}](https://cran.r-project.org/package=incidence) package.
Unlike the original package, {incidence2} concentrates only on the
initial calculation, manipulation and plotting of the resultant
*incidence* objects.

## Installing the package

You can install the released version of {incidence2} from
[CRAN](https://cran.r-project.org/) with:

``` r
install.packages("incidence2")
```

<div class="pkgdown-devel">

The development version of {incidence2} can be installed from GitHub
with:

``` r
remotes::install_github("reconverse/incidence2", build_vignettes = TRUE)
```

</div>

## Vignettes

An overview of {incidence2} is provided in the vignette distributed with
the package:

-   `vignette("incidence2", package = "incidence2")`
