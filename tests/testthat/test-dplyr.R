library(dplyr)

data(ebola_sim_clean, package = "outbreaks")
dat <- ebola_sim_clean$linelist

# create incidence object
inci <-
  dat %>%
  incidence(
    date_index = date_of_onset,
    interval = "2 weeks",
    groups = c(hospital, gender),
    na_as_group = TRUE
  )

inci_month <-
  dat %>%
  incidence(
    date_index = date_of_onset,
    interval = "yrmon",
    groups = c(hospital, gender),
    na_as_group = TRUE
  )


inci_quarter <-
  dat %>%
  incidence(
    date_index = date_of_onset,
    interval = "yrqtr",
    groups = c(hospital, gender),
    na_as_group = TRUE
  )

inci_year <-
  dat %>%
  incidence(
    date_index = date_of_onset,
    interval = "yr",
    groups = c(hospital, gender),
    na_as_group = TRUE
  )



test_that("operations that preserve class", {
  x <-
    inci %>%
    filter(gender == "f", hospital == "Rokupa Hospital")

  expect_s3_class(x, "incidence2")

  x <-
    inci %>%
    slice_sample(prop = 0.1)

  expect_s3_class(x, "incidence2")

  x <-
    inci %>%
    slice_sample(n = 10)

  expect_s3_class(x, "incidence2")

  x <-
    inci %>%
    slice(1, 5, 10)

  expect_s3_class(x, "incidence2")

  x <-
    inci %>%
    slice_max(order_by = count, n = 10)

  expect_s3_class(x, "incidence2")

  x <-
    inci %>%
    select(everything())

  expect_s3_class(x, "incidence2")

  x <-
    inci %>%
    mutate(future = date_index + 999)

  expect_s3_class(x, "incidence2")

  x <-
    inci %>%
    rename(left_bin = date_index)

  expect_s3_class(x, "incidence2")

  expect_s3_class(inci[], "incidence2")

  # Adding rows that are multiple of 2 weeks maintains class
  x <-
    inci %>%
    slice_head(n = 2) %>%
    mutate(date_index = date_index + 112) %>%
    bind_rows(inci)
  expect_true(inherits(x, "incidence2"))


  # Adding rows that are first date of a month maintains class
  x <-
    inci_month %>%
    slice_head(n = 2) %>%
    mutate(date_index = as_yrmon("2020-03-01")) %>%
  bind_rows(inci_month)
  expect_true(inherits(x, "incidence2"))


  # Adding rows that are first date of a quarter maintains class
  x <-
    inci_quarter %>%
    slice_head(n = 2) %>%
    mutate(date_index = as_yrqtr("2020-07-01")) %>%
    bind_rows(inci_quarter)
  expect_true(inherits(x, "incidence2"))


  # Adding rows that are first date of a year maintains class
  x <-
    inci_year %>%
    slice_head(n = 2) %>%
    mutate(date_index = as_yr("2020-01-01")) %>%
    bind_rows(inci_year)
  expect_true(inherits(x, "incidence2"))


})

test_that("operations that drop class", {

  expect_false(inherits(inci[1:3], "incidence2"))


  x <-
    inci %>%
    select(date_index, count)
  expect_false(inherits(x, "incidence2"))

  x <-
    inci %>%
    select(-1)
  expect_false(inherits(x, "incidence2"))

  x <-
    inci %>%
    pull(1)

  expect_false(inherits(x, "incidence2"))

  x <-
    inci %>%
    transmute(new_count = count + 1)

  expect_false(inherits(x, "incidence2"))




# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# NOTE - These are now covered by grouped date checks
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

  # Changing rows to have wrong interval (e.g. not 2 weeks) drops class
  # x <- inci
  # x[1, "date_index"] <- as_period(Sys.Date(), interval = "3 weeks")
  #
  # expect_false(inherits(x, "incidence2"))

  # Adding rows with dates that are not multiples of 2 weeks drops class
  # x <-
  #   inci %>%
  #   slice_head(n = 2) %>%
  #   mutate(date_index = date_index + 30) %>%
  #   bind_rows(inci)
  # expect_false(inherits(x, "incidence2"))
  #
  # # Adding rows that are not first day of a month drops class
  # y <-
  #   inci_month %>%
  #   slice_head(n = 2) %>%
  #   mutate(date_index = as.Date("2020-03-02"))
  #   y %>% bind_rows(inci_month)
  # expect_false(inherits(x, "incidence2"))
  #
  # # Adding rows that are not the first date of a quarter drops class
  # x <-
  #   inci_quarter %>%
  #   slice_head(n = 2) %>%
  #   mutate(date_index = as.Date("2020-06-01")) %>%
  #   bind_rows(inci_quarter)
  # expect_false(inherits(x, "incidence2"))
  #
  # # Adding rows that are not the first date of a year drops class
  # x <-
  #   inci_year %>%
  #   slice_head(n = 2) %>%
  #   mutate(date_index = as.Date("2020-02-01")) %>%
  #   bind_rows(inci_year)
  # expect_false(inherits(x, "incidence2"))
  #
  #
  # x <- inci
  # x[1,1] <- x[1,1] + 3
  # expect_false(inherits(x, "incidence2"))


})
