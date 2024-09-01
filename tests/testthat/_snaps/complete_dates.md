# Fails with good error for bad input

    Code
      complete_dates(dat)
    Condition
      Error in `complete_dates()`:
      ! `dat` is not an 'incidence2' object.

---

    Code
      complete_dates(i, expand = "bob")
    Condition
      Error in `complete_dates()`:
      ! `expand` must be boolean (TRUE/FALSE).

---

    Code
      complete_dates(i, allow_POSIXct = "bob")
    Condition
      Error in `complete_dates()`:
      ! `allow_POSIXct` must be boolean (TRUE/FALSE).

---

    Code
      complete_dates(i, fill = 1:2)
    Condition
      Error in `complete_dates()`:
      ! `fill` must be of lenth 1.

---

    Code
      complete_dates(i, by = 2)
    Condition
      Error in `complete_dates()`:
      ! `by` argument is now Defunct. Setting `by = 1L` or `by = 1` is permitted for compatibility only.

---

    Code
      complete_dates(i)
    Condition
      Error in `complete_dates()`:
      ! <POSIXct> date_index columns detected. Internally <POSIXct> objects are represented as seconds since the UNIX epoch and calling `complete_dates()` on an object of granularity can lead to significant memory usage. If you are sure you wish to do this, please call again with the argument `allow_POSIXct` set to `TRUE`. If this level of aggregation is not desired, consider recreating the <incidence> object using <Dates> for daily incidence. This can be done prior to calling `incidence()` or, alternatively, by setting the argument `interval = 'day'` within the call itself.

