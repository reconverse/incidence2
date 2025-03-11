# Fails for bad input

    Code
      keep_first(x, 3)
    Condition
      Error in `keep_first()`:
      ! `keep_first` does not work if a column is already named 'tmp___index'. Please rename and try again. If this is problematic, please raise an issue.

---

    Code
      keep_last(x, 3)
    Condition
      Error in `keep_last()`:
      ! `keep_last` does not work if a column is already named 'tmp___index'. Please rename and try again. If this is problematic, please raise an issue.

---

    Code
      keep_first("bob")
    Condition
      Error in `keep_first()`:
      ! `x` must be an incidence2 object.

---

    Code
      keep_last("bob")
    Condition
      Error in `keep_last()`:
      ! `x` must be an incidence2 object.

