# split works

    Code
      split(dat, f = dat$hospital)
    Condition
      Error in `split.incidence2()`:
      ! `f` argument cannot be used in `split.incidence2()` as the groupings are implicit.

---

    Code
      split(dat, drop = TRUE)
    Condition
      Error in `split.incidence2()`:
      ! `drop` argument cannot be used with `split.incidence2()`.

