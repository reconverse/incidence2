# regroup works

    Code
      regroup("test")
    Condition
      Error in `regroup()`:
      ! `x` must be an <incidence2> object.

---

    Code
      regroup(incidence(dat, date_index = "dates", groups = "group_1"), groups = character())
    Condition
      Error in `regroup()`:
      ! `groups` must be NULL or a character vector.

---

    Code
      regroup(incidence(dat, date_index = "dates", groups = "group_1"), groups = 1)
    Condition
      Error in `regroup()`:
      ! `groups` must be NULL or a character vector.

---

    Code
      regroup(incidence(dat, date_index = "dates", groups = "group_1"), groups = "bob")
    Condition
      Error in `regroup()`:
      ! Not all variables from `groups` are groupings of `x`.

