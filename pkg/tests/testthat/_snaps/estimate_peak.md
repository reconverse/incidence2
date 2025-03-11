# Fails with good error for bad input

    Code
      estimate_peak(DATES)
    Condition
      Error in `estimate_peak()`:
      ! `DATES` is not an 'incidence2' object.

---

    Code
      estimate_peak(i, n = 1.5)
    Condition
      Error in `estimate_peak()`:
      ! `n` must be integerish and of length 1.

---

    Code
      estimate_peak(i, alpha = NA_real_)
    Condition
      Error in `estimate_peak()`:
      ! `alpha` must be a numeric vector of length 1 and not NA.

---

    Code
      estimate_peak(i, alpha = "bob")
    Condition
      Error in `estimate_peak()`:
      ! `alpha` must be a numeric vector of length 1 and not NA.

---

    Code
      estimate_peak(i, first_only = "bob")
    Condition
      Error in `estimate_peak()`:
      ! `first_only` must be boolean (TRUE/FALSE).

---

    Code
      estimate_peak(i, progress = "bob")
    Condition
      Error in `estimate_peak()`:
      ! `progress` must be boolean (TRUE/FALSE).

