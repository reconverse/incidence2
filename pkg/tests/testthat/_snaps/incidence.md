# Fails with good error for bad input

    Code
      incidence(dummy)
    Condition
      Error in `incidence()`:
      ! `x` must be a data frame.

---

    Code
      incidence(dat, date_index = character())
    Condition
      Error in `incidence()`:
      ! `date_index` must be a character vector of length 1 or more.

---

    Code
      incidence(dat, date_index = 1L)
    Condition
      Error in `incidence()`:
      ! `date_index` must be a character vector of length 1 or more.

---

    Code
      incidence(dat, date_index = "bob")
    Condition
      Error in `incidence()`:
      ! Not all variables from `date_index` are present in `x`.

---

    Code
      incidence(dat, date_index = "dates", counts = character())
    Condition
      Error in `incidence()`:
      ! `counts` must be NULL or a character vector of length 1 or more.

---

    Code
      incidence(dat, date_index = "dates", counts = 1L)
    Condition
      Error in `incidence()`:
      ! `counts` must be NULL or a character vector of length 1 or more.

---

    Code
      incidence(dat, date_index = c("dates", "dates2"), counts = "count")
    Condition
      Error in `incidence()`:
      ! If `counts` is specified `date_index` must be of length 1.

---

    Code
      incidence(dat, date_index = c("dates", "dates"), counts = "count")
    Condition
      Error in `incidence()`:
      ! `date_index` values must be unique.

---

    Code
      incidence(dat, date_index = "dates", counts = "bob")
    Condition
      Error in `incidence()`:
      ! Not all variables from `counts` are present in `x`.

---

    Code
      incidence(dat, date_index = "dates", counts = "count", groups = 1)
    Condition
      Error in `incidence()`:
      ! `groups` must be NULL or a character vector.

---

    Code
      incidence(dat, c("dates", "dates2"), fill = 9)
    Condition
      Error in `incidence()`:
      ! `fill` can only be given when `complete_dates = TRUE`.

---

    Code
      incidence(dat2, date_index = "dates")
    Condition
      Error in `incidence()`:
      ! POSIXlt date_index columns are not currently supported.

# Warning is good for non-whole dates

    Code
      incidence(dat, date_index = "dates")
    Condition
      Warning in `incidence()`:
      Non-whole <Date> columns detected. These can be confusing as they are displayed without the fractional element and can also cause oddities when plotting. If you are interested in daily incidence, consider removing the fractional part. This can be done prior to calling `incidence()` or, alternatively, by setting the argument `interval = 'day'` within the call itself.
    Output
      # incidence:  10 x 3
      # count vars: dates
         date_index count_variable count
         <date>     <chr>          <int>
       1 2025-04-12 dates              1
       2 2025-04-13 dates              1
       3 2025-04-14 dates              1
       4 2025-04-15 dates              1
       5 2025-04-16 dates              1
       6 2025-04-17 dates              1
       7 2025-04-18 dates              1
       8 2025-04-19 dates              1
       9 2025-04-20 dates              1
      10 2025-04-21 dates              1

