# regroup_ works

    Code
      regroup_("test")
    Condition
      Error in `regroup_()`:
      ! `x` must be an <incidence2> object.

---

    Code
      regroup_(incidence(dat, date_index = "dates", groups = "group_1"), groups = bob)
    Condition
      Error in `regroup_()`:
      ! Can't select columns that don't exist.
      x Column `bob` doesn't exist.

