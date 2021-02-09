
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
[![Codecov test
coverage](https://codecov.io/gh/reconhub/incidence2/branch/master/graph/badge.svg)](https://codecov.io/gh/reconhub/incidence2?branch=master)
[![R-CMD-check](https://github.com/reconhub/incidence2/workflows/R-CMD-check/badge.svg)](https://github.com/reconhub/incidence2/actions)
<!-- badges: end -->

# Scope

*incidence2* refocusses the scope of the original
[incidence](https://github.com/reconhub/incidence) package. The aim is
to provide a “tidy” interface for users to work with whilst at the same
time simplifying the underlying implementation. To this end,
*incidence2* concentrates only on the initial data handling, calculation
and graphing of incidence objects. The “fitting” and “peak estimation”
functions of [incidence](https://github.com/reconhub/incidence)
(e.g. `incidence::fit` and `incidence::estimate_peak`) are being
implemented in an accompanying package called
[`incidence2plus`](https://github.com/reconhub/incidence2extra). Here
they will have a more consistent interface, better choice of underlying
models, and tidier outputs.

# What does it do?

The main features of the package include:

  - **`incidence()`**: compute incidence from both linelist and
    pre-aggregated datasets; any fixed time interval can be used; the
    returned object is a tibble subclass called *incidence2*.

  - plotting functions **`plot()`** and **`facet_plot()`**: these
    functions return customised ggplot2 plots of *incidence2* objects
    (see **`plot.incidence2()`** for details).

  - Compatible with [dplyr](https://dplyr.tidyverse.org/) for data
    manipulation. (see `vignette("handling_incidence_objects")` for more
    details).

  - **`regroup()`**: regroup incidence from different groups into one
    global incidence time series.

  - **`cumulate()`**: computes cumulative incidence over time from an
    `incidence()` object.

  - **`print()`** and **`summary()`** functions.

  - Conversion functions:
    
      - **`as.data.frame()`**: converts an `incidence()` object into a
        `data.frame`.
    
      - **`as_tibble()`**: converts an `incidence()` object into a
        `tibble`.

  - Accessor functions: **`get_counts_name()`**, **`get_dates_name()`**,
    **`get_dates()`**, **`get_group_names()`**, **`get_interval()`**,
    **`get_timespan()`** and **`get_n()`**.

# Installing the package

You can install the current version of the package from
[CRAN](https://cran.r-project.org/) with:

``` r
install.packages("incidence2")
```

The development version can be installed from
[GitHub](https://github.com/) with:

``` r
if (!require(remotes)) {
  install.packages("remotes")
}
remotes::install_github("reconhub/incidence2", build_vignettes = TRUE)
```

# Resources

## Vignettes

A short overview of *incidence2* is provided below in the worked example
below. More detailed tutorials are distributed as vignettes with the
package:

  - `vignette("Introduction", package = "incidence2")`
  - `vignette("handling_incidence_objects", package = "incidence2")`
  - `vignette("customizing_incidence_plots", package = "incidence2")`

## Websites

The following websites are available:

  - The *incidence2* project on *github*, useful for developers,
    contributors, and users wanting to post issues, bug reports and
    feature requests: <br> <https://github.com/reconhub/incidence2>

## Getting help online

Bug reports and feature requests should be posted on *github* using the
[*issue* system](https://github.com/reconhub/incidence2/issues). All
other questions should be posted on the **RECON** slack channel see
<https://www.repidemicsconsortium.org/forum/> for details on how to
join.

# A quick overview

This short example uses the simulated Ebola Virus Disease (EVD) outbreak
from the package [*outbreaks*](https://github.com/reconhub/outbreaks).
It shows how to compute incidence for various time steps plot the
resulting incidence tables.

First, we load the data:

``` r
library(outbreaks)
library(incidence2)

dat <- ebola_sim_clean$linelist
str(dat)
#> 'data.frame':    5829 obs. of  11 variables:
#>  $ case_id                : chr  "d1fafd" "53371b" "f5c3d8" "6c286a" ...
#>  $ generation             : int  0 1 1 2 2 0 3 3 2 3 ...
#>  $ date_of_infection      : Date, format: NA "2014-04-09" ...
#>  $ date_of_onset          : Date, format: "2014-04-07" "2014-04-15" ...
#>  $ date_of_hospitalisation: Date, format: "2014-04-17" "2014-04-20" ...
#>  $ date_of_outcome        : Date, format: "2014-04-19" NA ...
#>  $ outcome                : Factor w/ 2 levels "Death","Recover": NA NA 2 1 2 NA 2 1 2 1 ...
#>  $ gender                 : Factor w/ 2 levels "f","m": 1 2 1 1 1 1 1 1 2 2 ...
#>  $ hospital               : Factor w/ 5 levels "Connaught Hospital",..: 2 1 3 NA 3 NA 1 4 3 5 ...
#>  $ lon                    : num  -13.2 -13.2 -13.2 -13.2 -13.2 ...
#>  $ lat                    : num  8.47 8.46 8.48 8.46 8.45 ...
```

## Computing and plotting incidence

We compute the weekly incidence:

``` r
i_year <- incidence(dat, date_index = date_of_onset, interval = "year")
i_year
#> An incidence2 object: 56 x 2
#> [5829 cases from days 2014-W15 to 2015-W18]
#> [interval: 1 year]
#> [cumulative: FALSE]
#> 
#>    date_index count
#>    <yrwk>     <int>
#>  1 2014-W15       1
#>  2 2014-W16       1
#>  3 2014-W17       5
#>  4 2014-W18       4
#>  5 2014-W19      12
#>  6 2014-W20      17
#>  7 2014-W21      15
#>  8 2014-W22      19
#>  9 2014-W23      23
#> 10 2014-W24      21
#> # … with 46 more rows
summary(i_year)
#> An incidence2 object: 56 x 2
#> 5829 cases from 2014-W15 to 2015-W18
#> interval: 1 year
#> cumulative: FALSE
#> timespan: 392 days
plot(i_year)
```

<img src="man/figures/README-incid7-1.png" style="display: block; margin: auto;" />

`incidence()` can also compute incidence by specified groups using the
`groups` argument. For instance, we can compute the weekly incidence by
gender and plot in a single, stacked chart:

``` r
i_year_sex <- incidence(dat, interval = "year", date_index = date_of_onset,
                        groups = gender)
i_year_sex
#> An incidence2 object: 109 x 3
#> [5829 cases from days 2014-W15 to 2015-W18]
#> [interval: 1 year]
#> [cumulative: FALSE]
#> 
#>    date_index gender count
#>    <yrwk>     <fct>  <int>
#>  1 2014-W15   f          1
#>  2 2014-W16   m          1
#>  3 2014-W17   f          4
#>  4 2014-W17   m          1
#>  5 2014-W18   f          4
#>  6 2014-W19   f          9
#>  7 2014-W19   m          3
#>  8 2014-W20   f          7
#>  9 2014-W20   m         10
#> 10 2014-W21   f          8
#> # … with 99 more rows
summary(i_year_sex)
#> An incidence2 object: 109 x 3
#> 5829 cases from 2014-W15 to 2015-W18
#> interval: 1 year
#> cumulative: FALSE
#> timespan: 392 days
#> 
#> 1 grouped variable
#> 
#>   gender count
#> * <fct>  <int>
#> 1 f       2934
#> 2 m       2895
plot(i_year_sex, fill = "gender")
```

<img src="man/figures/README-genderstack-1.png" style="display: block; margin: auto;" />

we can facet our plot (grouping detected automatically):

``` r
facet_plot(i_year_sex, n_breaks = 6)
```

<img src="man/figures/README-genderfacet-1.png" style="display: block; margin: auto;" />

and we can also group by multiple variables specifying different facets
and fills:

``` r
# incidence is compatible with the magrittr pipe operator
i_year_sh <- incidence(dat, date_index = date_of_onset, interval = "year",
                       groups = c(gender, hospital))
i_year_sh
#> An incidence2 object: 601 x 4
#> [5829 cases from days 2014-W15 to 2015-W18]
#> [interval: 1 year]
#> [cumulative: FALSE]
#> 
#>    date_index gender hospital                                     count
#>    <yrwk>     <fct>  <fct>                                        <int>
#>  1 2014-W15   f      Military Hospital                                1
#>  2 2014-W16   m      Connaught Hospital                               1
#>  3 2014-W17   f      other                                            2
#>  4 2014-W17   f      <NA>                                             2
#>  5 2014-W17   m      other                                            1
#>  6 2014-W18   f      Connaught Hospital                               1
#>  7 2014-W18   f      Princess Christian Maternity Hospital (PCMH)     1
#>  8 2014-W18   f      Rokupa Hospital                                  1
#>  9 2014-W18   f      <NA>                                             1
#> 10 2014-W19   f      Connaught Hospital                               2
#> # … with 591 more rows
summary(i_year_sh)
#> An incidence2 object: 601 x 4
#> 5829 cases from 2014-W15 to 2015-W18
#> interval: 1 year
#> cumulative: FALSE
#> timespan: 392 days
#> 
#> 2 grouped variables
#> 
#>   gender count
#> * <fct>  <int>
#> 1 f       2934
#> 2 m       2895
#> 
#> 
#>   hospital                                     count
#> * <fct>                                        <int>
#> 1 Connaught Hospital                            1737
#> 2 Military Hospital                              889
#> 3 other                                          876
#> 4 Princess Christian Maternity Hospital (PCMH)   420
#> 5 Rokupa Hospital                                451
#> 6 <NA>                                          1456
facet_plot(i_year_sh, facets = gender, fill = hospital, n_breaks = 6)
```

<img src="man/figures/README-genderhospital-1.png" style="display: block; margin: auto;" />
