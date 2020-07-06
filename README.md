
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

**Disclaimer:** this package is a work in progress. Please reach out to
the authors before using.

# Scope

*incidence2* refocusses the scope of the original
[incidence](https://github.com/reconhub/incidence) package. The aim is
to provide a “tidy” interface for users to work with whilst at the same
time simplifying the underlying implementation. To this end,
*incidence2* concentrates only on the initial data handling, calculation
and graphing of incidence objects. The “fitting” functions of
[incidence](https://github.com/reconhub/incidence)
(e.g. `incidence::fit`) will likely find there place in a smaller,
separate, package with a more consistent interface, more choice of
underlying models, and tidier outputs.

# Installing the package

To install the development, *github* version of the package use:

``` r
devtools::install_github("reconhub/incidence2")
```

# What does it do?

The main features of the package include:

  - **`incidence()`**: compute incidence from linelist datasets; any
    fixed time interval can be used; the returned object is a tibble
    subclass called *incidence*.

  - plotting functions **`plot()`** and **`facet_plot()`**: these
    functions return customised ggplot2 plots of *incidence* objects
    (see `?plot.incidence` for details).

  - Compatible with [dplyr](https://dplyr.tidyverse.org/) for data
    manipulation.

  - **`regroup()`**: regroup incidence from different groups into one
    global incidence time series.

  - **`cumulate()`**: computes cumulative incidence over time from an
    `incidence` object.

  - **`bootstrap()`**: generates a bootstrapped *incidence()* object by
    re-sampling, with replacement, the original dates of events.

  - **`find_peak()`**: locates the peak time of the epicurve.

  - **`estimate_peak()`**: uses bootstrap to estimate the peak time (and
    related confidence interval) of a partially observed outbreak.

  - **`print()`** and **`summary()`** functions.

  - Conversion functions:
    
      - **`as.data.frame()`**: converts an `incidence()` object into a
        `data.frame`.
    
      - **`as_tibble()`**: convertss an `incidence()` object into a
        `tibble`.

  - Accessor functions: **`get_count_vars()`**, **`get_date_vars()`**,
    **`get_group_vars()`**, **`get_interval()`**, **`get_timespan()`**
    and **`get_n()`**.

# A quick overview

The following worked example provides a brief overview of the package’s
functionalities.

## Loading the data

This example uses the simulated Ebola Virus Disease (EVD) outbreak from
the package [*outbreaks*](https://github.com/reconhub/outbreaks). It
shows how to compute incidence for various time steps and manipulate the
data.

First, we load the data:

``` r
library(outbreaks)
library(incidence2)
library(dplyr)

dat <- ebola_sim_clean$linelist
glimpse(dat)
#> Rows: 5,829
#> Columns: 11
#> $ case_id                 <chr> "d1fafd", "53371b", "f5c3d8", "6c286a", "0f58…
#> $ generation              <int> 0, 1, 1, 2, 2, 0, 3, 3, 2, 3, 4, 3, 4, 2, 4, …
#> $ date_of_infection       <date> NA, 2014-04-09, 2014-04-18, NA, 2014-04-22, …
#> $ date_of_onset           <date> 2014-04-07, 2014-04-15, 2014-04-21, 2014-04-…
#> $ date_of_hospitalisation <date> 2014-04-17, 2014-04-20, 2014-04-25, 2014-04-…
#> $ date_of_outcome         <date> 2014-04-19, NA, 2014-04-30, 2014-05-07, 2014…
#> $ outcome                 <fct> NA, NA, Recover, Death, Recover, NA, Recover,…
#> $ gender                  <fct> f, m, f, f, f, f, f, f, m, m, f, f, f, f, f, …
#> $ hospital                <fct> Military Hospital, Connaught Hospital, other,…
#> $ lon                     <dbl> -13.21799, -13.21491, -13.22804, -13.23112, -…
#> $ lat                     <dbl> 8.473514, 8.464927, 8.483356, 8.464776, 8.452…
```

## Computing and plotting incidence

We compute the weekly incidence:

``` r
i_7 <- incidence(dat, date_index = date_of_onset, interval = 7)
i_7
#> <incidence object>
#> [5829 cases from days 2014-04-07 to 2015-04-27]
#> [interval: 7 days]
#> [cumulative: FALSE]
#> 
#>    date_group weeks    isoweeks count
#>    <date>     <aweek>  <chr>    <int>
#>  1 2014-04-07 2014-W15 2014-W15     1
#>  2 2014-04-14 2014-W16 2014-W16     1
#>  3 2014-04-21 2014-W17 2014-W17     5
#>  4 2014-04-28 2014-W18 2014-W18     4
#>  5 2014-05-05 2014-W19 2014-W19    12
#>  6 2014-05-12 2014-W20 2014-W20    17
#>  7 2014-05-19 2014-W21 2014-W21    15
#>  8 2014-05-26 2014-W22 2014-W22    19
#>  9 2014-06-02 2014-W23 2014-W23    23
#> 10 2014-06-09 2014-W24 2014-W24    21
#> # … with 46 more rows
summary(i_7)
#> <incidence object>
#> 
#> 5829 cases from days 2014-04-07 to 2015-04-27
#> interval: 7 days
#> cumulative: FALSE
#> timespan: 386 days
plot(i_7, color = "black")
```

![](figs/incid7-1.png)<!-- -->

`incidence()` can also compute incidence by specified groups using the
`groups` argument. For instance, we can compute the weekly incidence by
gender and plot in a single, stacked chart:

``` r
i_7_sex <- incidence(dat, interval = "week", 
                     date_index = date_of_onset, groups = gender)
i_7_sex
#> <incidence object>
#> [5829 cases from days 2014-04-07 to 2015-04-27]
#> [interval: 1 week]
#> [cumulative: FALSE]
#> 
#>    date_group weeks    isoweeks gender count
#>    <date>     <aweek>  <chr>    <fct>  <int>
#>  1 2014-04-07 2014-W15 2014-W15 f          1
#>  2 2014-04-07 2014-W15 2014-W15 m          0
#>  3 2014-04-14 2014-W16 2014-W16 f          0
#>  4 2014-04-14 2014-W16 2014-W16 m          1
#>  5 2014-04-21 2014-W17 2014-W17 f          5
#>  6 2014-04-21 2014-W17 2014-W17 m          0
#>  7 2014-04-28 2014-W18 2014-W18 f          2
#>  8 2014-04-28 2014-W18 2014-W18 m          2
#>  9 2014-05-05 2014-W19 2014-W19 f          9
#> 10 2014-05-05 2014-W19 2014-W19 m          3
#> # … with 102 more rows
summary(i_7_sex)
#> <incidence object>
#> 
#> 5829 cases from days 2014-04-07 to 2015-04-27
#> interval: 1 week
#> cumulative: FALSE
#> timespan: 386 days
#> 
#> 1 grouped variable
#> 
#>   gender count
#>   <fct>  <int>
#> 1 f       2934
#> 2 m       2895
plot(i_7_sex, stack = TRUE, color = "black")
```

![](figs/genderstack-1.png)<!-- -->

we can facet our plot:

``` r
facet_plot(i_7_sex, n_breaks = 4)
```

![](figs/genderfacet-1.png)<!-- -->

and we can also group by multiple variables

``` r
# incidence is compatible with the magrittr pipe operator
i_7_sh <- 
  dat %>% 
  incidence(date_index = date_of_onset, interval = "week", 
            groups = c(gender, hospital))
i_7_sh
#> <incidence object>
#> [5829 cases from days 2014-04-07 to 2015-04-27]
#> [interval: 1 week]
#> [cumulative: FALSE]
#> 
#>    date_group weeks    isoweeks gender hospital                            count
#>    <date>     <aweek>  <chr>    <fct>  <fct>                               <int>
#>  1 2014-04-07 2014-W15 2014-W15 f      Military Hospital                       1
#>  2 2014-04-07 2014-W15 2014-W15 m      Military Hospital                       0
#>  3 2014-04-07 2014-W15 2014-W15 f      Connaught Hospital                      0
#>  4 2014-04-07 2014-W15 2014-W15 m      Connaught Hospital                      0
#>  5 2014-04-07 2014-W15 2014-W15 f      other                                   0
#>  6 2014-04-07 2014-W15 2014-W15 m      other                                   0
#>  7 2014-04-07 2014-W15 2014-W15 f      <NA>                                    0
#>  8 2014-04-07 2014-W15 2014-W15 m      <NA>                                    0
#>  9 2014-04-07 2014-W15 2014-W15 f      Princess Christian Maternity Hospi…     0
#> 10 2014-04-07 2014-W15 2014-W15 m      Princess Christian Maternity Hospi…     0
#> # … with 662 more rows
i_7_sh %>% summary()
#> <incidence object>
#> 
#> 5829 cases from days 2014-04-07 to 2015-04-27
#> interval: 1 week
#> cumulative: FALSE
#> timespan: 386 days
#> 
#> 2 grouped variables
#> 
#>   gender count
#>   <fct>  <int>
#> 1 f       2934
#> 2 m       2895
#> 
#> 
#>   hospital                                     count
#>   <fct>                                        <int>
#> 1 Connaught Hospital                            1737
#> 2 Military Hospital                              889
#> 3 other                                          876
#> 4 Princess Christian Maternity Hospital (PCMH)   420
#> 5 Rokupa Hospital                                451
#> 6 <NA>                                          1456
i_7_sh %>% facet_plot(n_breaks = 2, nrow = 4)
```

![](figs/genderhospital-1.png)<!-- -->

## Handling `incident` objects

`incidence` objects can be manipulated easily using both operators from
base R and dplyr.. The `[` operator implements normal subsetting of rows
and columns. For instance, to keep only the first 20 weeks of the
epidemic:

``` r
plot(i_7[1:20, ])
```

![](figs/start-1.png)<!-- -->

incidence2 extends common dplyr verbs and this allows easy temporal
subsetting:

``` r
i_7 %>% 
  filter(date_group >= as.Date("2015-01-01")) %>% 
  plot(color = "white")
```

![](figs/tail-1.png)<!-- -->

Subsetting groups can also matter. For instance, let’s try and visualise
the incidence based on onset of symptoms by outcome:

``` r
i_7_outcome <- incidence(dat,
                         date_index = date_of_onset,
                         interval = 7, 
                         groups = outcome)
i_7_outcome
#> <incidence object>
#> [5829 cases from days 2014-04-07 to 2015-04-27]
#> [interval: 7 days]
#> [cumulative: FALSE]
#> 
#>    date_group weeks    isoweeks outcome count
#>    <date>     <aweek>  <chr>    <fct>   <int>
#>  1 2014-04-07 2014-W15 2014-W15 <NA>        1
#>  2 2014-04-07 2014-W15 2014-W15 Death       0
#>  3 2014-04-07 2014-W15 2014-W15 Recover     0
#>  4 2014-04-14 2014-W16 2014-W16 <NA>        1
#>  5 2014-04-14 2014-W16 2014-W16 Death       0
#>  6 2014-04-14 2014-W16 2014-W16 Recover     0
#>  7 2014-04-21 2014-W17 2014-W17 <NA>        1
#>  8 2014-04-21 2014-W17 2014-W17 Death       1
#>  9 2014-04-21 2014-W17 2014-W17 Recover     3
#> 10 2014-04-28 2014-W18 2014-W18 <NA>        1
#> # … with 158 more rows
summary(i_7_outcome)
#> <incidence object>
#> 
#> 5829 cases from days 2014-04-07 to 2015-04-27
#> interval: 7 days
#> cumulative: FALSE
#> timespan: 386 days
#> 
#> 1 grouped variable
#> 
#>   outcome count
#>   <fct>   <int>
#> 1 Death    2564
#> 2 Recover  1963
#> 3 <NA>     1302
plot(i_7_outcome, stack = TRUE)
```

![](figs/i7outcome-1.png)<!-- -->

``` r
facet_plot(i_7_outcome, n_breaks = 3)
```

![](figs/i7outcome-2.png)<!-- -->

``` r
i_7_outcome_cum <- cumulate(i_7_outcome)
i_7_outcome_cum
#> <incidence object>
#> [155032 cases from days 2014-04-07 to 2015-04-27]
#> [interval: 7 days]
#> [cumulative: TRUE]
#> 
#>    date_group weeks    isoweeks outcome count
#>    <date>     <aweek>  <chr>    <fct>   <int>
#>  1 2014-04-07 2014-W15 2014-W15 <NA>        1
#>  2 2014-04-07 2014-W15 2014-W15 Death       0
#>  3 2014-04-07 2014-W15 2014-W15 Recover     0
#>  4 2014-04-14 2014-W16 2014-W16 <NA>        2
#>  5 2014-04-14 2014-W16 2014-W16 Death       0
#>  6 2014-04-14 2014-W16 2014-W16 Recover     0
#>  7 2014-04-21 2014-W17 2014-W17 <NA>        3
#>  8 2014-04-21 2014-W17 2014-W17 Death       1
#>  9 2014-04-21 2014-W17 2014-W17 Recover     3
#> 10 2014-04-28 2014-W18 2014-W18 <NA>        4
#> # … with 158 more rows
plot(i_7_outcome_cum)
```

![](figs/i7outcome_cum-1.png)<!-- -->

Groups can also be collapsed into a single time series using `regroup`:

``` r
i_regrouped <- regroup(i_7_outcome)
i_regrouped
#> <incidence object>
#> [5829 cases from days 2014-04-07 to 2015-04-27]
#> [interval: 7 days]
#> [cumulative: FALSE]
#> 
#>    date_group weeks    isoweeks count
#>    <date>     <aweek>  <chr>    <int>
#>  1 2014-04-07 2014-W15 2014-W15     1
#>  2 2014-04-14 2014-W16 2014-W16     1
#>  3 2014-04-21 2014-W17 2014-W17     5
#>  4 2014-04-28 2014-W18 2014-W18     4
#>  5 2014-05-05 2014-W19 2014-W19    12
#>  6 2014-05-12 2014-W20 2014-W20    17
#>  7 2014-05-19 2014-W21 2014-W21    15
#>  8 2014-05-26 2014-W22 2014-W22    19
#>  9 2014-06-02 2014-W23 2014-W23    23
#> 10 2014-06-09 2014-W24 2014-W24    21
#> # … with 46 more rows
identical(i_7, i_regrouped)
#> [1] TRUE
```
