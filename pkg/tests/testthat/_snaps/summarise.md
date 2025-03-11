# summarise works

    Code
      summarise(dat, .by = "hospital")
    Condition
      Error in `summarise.incidence2()`:
      ! `.by` argument cannot be used in `summary.incidence2()` as the groupings are implicit.

---

    Code
      summarise(dat, .groups = "keep")
    Condition
      Error in `summarise.incidence2()`:
      ! `.groups` argument cannot be used with `summary.incidence2()`.

