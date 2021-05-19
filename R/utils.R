# check for suggested packages --------------------------------------------
check_suggests <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    abort(sprintf("Suggested package '%s' not present.", package))
  }
}

# -------------------------------------------------------------------------
# new_bare_tibble() is a small wrapper around tibble::new_tibble() that also
# forces extra attributes to be dropped through the use of
# vctrs::new_data_frame(). In the future, new_tibble() might have an option
# to do this directly. See:
# https://github.com/DavisVaughan/2020-06-01_dplyr-vctrs-compat
new_bare_tibble <- function(x) {
  # Strips all attributes off `x` since `new_tibble()` currently doesn't
  x <- vctrs::new_data_frame(x)
  tibble::new_tibble(x, nrow = nrow(x))
}
# -------------------------------------------------------------------------


get_weekday_name <- function(x) {
  wdays <- weekdays(as.Date(as_yearweek(as.Date("2020-01-01"), firstday = 1L)) + 0:6)
  wdays <- setNames(1:7, wdays)
  names(wdays[x])
}
