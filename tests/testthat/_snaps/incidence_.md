# Fails with good error for bad input

    Code
      incidence_("bob")
    Condition
      Error in `incidence_()`:
      ! Can't select within an unnamed vector.

---

    Code
      incidence_(dat, date_index = character())
    Condition
      Error in `incidence_()`:
      ! `date_index` must be of length 1 or more.

---

    Code
      incidence_(dat, date_index = bob)
    Condition
      Error in `incidence_()`:
      ! Can't select columns that don't exist.
      x Column `bob` doesn't exist.

---

    Code
      incidence_(dat, date_index = dates, counts = character())
    Condition
      Error in `incidence_()`:
      ! `counts` must be NULL or a column in `x`.

---

    Code
      incidence_(dat, date_index = c(dates, dates2), counts = count)
    Condition
      Error in `incidence_()`:
      ! If `counts` is specified `date_index` must be of length 1.

---

    Code
      incidence_(dat, date_index = dates, counts = bob)
    Condition
      Error in `incidence_()`:
      ! Can't select columns that don't exist.
      x Column `bob` doesn't exist.

---

    Code
      incidence_(dat, date_index = dates, counts = count, groups = 1)
    Condition
      Error in `incidence_()`:
      ! `date_index` columns must be distinct from `groups`.

---

    Code
      incidence_(dat2, date_index = dates)
    Condition
      Error in `incidence()`:
      ! POSIXlt date_index columns are not currently supported.

