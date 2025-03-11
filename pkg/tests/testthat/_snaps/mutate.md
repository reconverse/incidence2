# mutate works

    Code
      mutate(dat, ave = data.table::frollmean(count, n = 3L, align = "right"), .by = "hospital")
    Condition
      Error in `mutate.incidence2()`:
      ! `.by` argument is not used in `mutate.incidence2()` as the groupings are implicit.

