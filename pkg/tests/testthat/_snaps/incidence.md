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

