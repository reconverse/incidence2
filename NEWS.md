# incidence2 2.0.0

Version 2.0.0 is a major, breaking release of incidence2. We have undertaken a
significant refactor that both simplifies the underlying code base and makes the
user interface more consistent and robust. Although the main changes are
highlighted below, users are strongly advised to read through the accompanying
documentation and vignettes.

## breaking changes

* We no longer support NSE (e.g. tidyselect semantics) within the package. Our
  motivation for removing support for NSE are both the complexity it can bring
  to the underlying code (making long term maintenance harder) and the
  complexity it can cause for other users / developers who want to build on
  top of incidence2.
  
* `new_incidence()`, `validate_incidence()`, `build_incidence()`, `get_n()`,
  `get_interval()`, `get_timespan()` and `facet_plot()` are now defunct and
  will error if called.
  
* `complete_counts()` is now renamed `complete_dates()` and gains two new
  parameters, `expand` and `by`. If `expand` is TRUE (default) then
  `complete_dates()` will attempt to use
  `function(x) seq(min(x), max(x), by = by)` to generate a complete sequence of
  dates.
  
* The `incidence()` function now always returns output in long format with
  dedicated columns for the count variables and values (set by arguments
  `count_names_to` and `count_values_to`).
  
* `incidence()` is now less flexible in what it can accept for the `interval`
  argument. For more complex date groupings users are encouraged to perform
  their require data transformations prior to calling `incidence()`.
  
* The default plotting of incidence objects as been greatly simplified. Sensible
  defaults have been chosen to enable a quick visual overview of incidence
  objects but users are advised to call ggplot2 directly for more bespoke
  plotting.
  
# incidence2 1.2.3

## New functions
* Reintroduces the `cumulate()` functionality (previously deprecated in 1.2.0).

## Breaking changes
* Default `fill` argument in `complete_counts()` is now 0 rather than NA.

# incidence2 1.2.2

## Bug fixes
* Fixes bug when input object to incidence is a data.table.

# incidence2 1.2.1

## Bug fixes
* Fixes bug in `incidence()` when more than one column was given for the date_index.
* Fixes incorrect test that did not take in to account changing time zones.

# incidence2 1.2.0

## New functions
* `new_incidence()`: A minimal incidence constructor.
* `validate_incidence()`: Check for internal consistency of incidence-like object.
* `build_incidence()`: Allows you to construct an incidence object whilst specifying
  your own date grouping function.
* `format.incidence()`
  
## Deprecated functions
* `cumulate()` will now give a deprecation error. We have removed the function
  to avoid users erroneously regressing against a cumulative count.

## Bug fixes
* Fixes bug in `incidence()` when dates were a character vector and the the
  default, daily, interval was specified.

## Other updates
* Now uses `dplyr` to handle list based columns (e.g. record-type objects from
  `vctrs`). For data.frames with only atomic columns, data.table is still used.
* Printing and summaries of incidence objects have been improved to remove
  duplication in the overview section. 
  

# incidence2 1.1.0

* New function `complete_counts()`.
* `plot()` and `facet_plot()` now have a `centre_dates` argument which can be
  set to `FALSE` to get histogram-esque date labels for single month, quarter
  and yearweek groupings.
* Internal refactoring due to breakages changes in the upstream grates package.


# incidence2 1.0.0
Due to multiple changes in the underlying representation of incidence2 objects
this release may possibly break old workflows particularly those relying on
the old implementations of date grouping:

* Now uses the package [`grates`](https://github.com/reconverse/grates) for
  date grouping.  This introduces the s3 classes `yrwk`, `yrmon`, `yrqtr`, `yr`,
  `period` and `int_period` as well as associated constructors which `incidence`
  now builds upon. As a result of this the
  [aweek](https://cran.r-project.org/package=aweek) dependency has been dropped.
* Add's `keep_first` and `keep_last` functions.
* Construction of `incidence` objects now faster due to underlying use of
  data.table.

# incidence2 0.2.2
* Fixes bug in get_interval.
* Removes message that was displayed when incidence class dropped.
* Refactoring of internal code to improve maintainability.
* Tests now use the 3rd edition of testthat. 

# incidence2 0.2.1
* Fixes bug in as.data.frame.incidence2
* Limits internal reliance on dplyr.

# incidence2 0.2.0

* Fixes issue with monthly incidence objects when `show_cases = TRUE` (see #42).
* Additional checks added for assessing whether a manipulated incidence object
  maintains its class.
* Improved implementation speed.
* NA's now ignored in the `count` variable of a pre-aggregated input to 
  `incidence` function.
* Fixes axis labelling and spacing.


# incidence2 0.1.0

* Initial release.
