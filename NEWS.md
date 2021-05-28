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
