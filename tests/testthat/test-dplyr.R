context("Extending dplyr")
library(dplyr)

data(ebola_sim_clean, package = "outbreaks")
dat <- ebola_sim_clean$linelist

# create incidence object
inci <-
  dat %>%
  incidence(
    date_index = date_of_onset,
    interval = "2 weeks",
    first_date = "2014-05-20",
    last_date = "2014-06-10",
    groups = c(hospital, gender),
    na_as_group = TRUE
  )

inci_month <-
  dat %>%
  incidence(
    date_index = date_of_onset,
    interval = "month",
    first_date = "2014-05-20",
    last_date = "2014-12-10",
    groups = c(hospital, gender),
    na_as_group = TRUE
  )


inci_quarter <-
  dat %>%
  incidence(
    date_index = date_of_onset,
    interval = "quarter",
    first_date = "2014-05-20",
    last_date = "2014-12-10",
    groups = c(hospital, gender),
    na_as_group = TRUE
  )

inci_year <-
  dat %>%
  incidence(
    date_index = date_of_onset,
    interval = "year",
    first_date = "2014-05-20",
    last_date = "2017-12-10",
    groups = c(hospital, gender),
    na_as_group = TRUE
  )



test_that("operations that preserve class", {
  x <-
    inci %>%
    filter(gender == "f", hospital == "Rokupa Hospital")

  expect_s3_class(x, "incidence")

  x <-
    inci %>%
    slice_sample(prop = 0.1)

  expect_s3_class(x, "incidence")

  x <-
    inci %>%
    slice_sample(n = 10)

  expect_s3_class(x, "incidence")

  x <-
    inci %>%
    slice(1, 5, 10)

  expect_s3_class(x, "incidence")

  x <-
    inci %>%
    slice_max(order_by = count, n = 10)

  expect_s3_class(x, "incidence")

  x <-
    inci %>%
    select(everything())

  expect_s3_class(x, "incidence")

  x <-
    inci %>%
    mutate(future = bin_date + 999)

  expect_s3_class(x, "incidence")

  x <-
    inci %>%
    rename(left_bin = bin_date)

  expect_s3_class(x, "incidence")

  expect_s3_class(inci[], "incidence")

  # Adding rows that are multiple of 2 weeks maintains class
  x <-
    inci %>%
    slice_head(n = 2) %>%
    mutate(bin_date = bin_date + 112) %>%
    bind_rows(inci)
  expect_true(inherits(x, "incidence"))


  # Adding rows that are first date of a month maintains class
  x <-
    inci_month %>%
    slice_head(n = 2) %>%
    mutate(bin_date = as.Date("2020-03-01")) %>%
  bind_rows(inci_month)
  expect_true(inherits(x, "incidence"))


  # Adding rows that are first date of a quarter maintains class
  x <-
    inci_quarter %>%
    slice_head(n = 2) %>%
    mutate(bin_date = as.Date("2020-07-01")) %>%
    bind_rows(inci_quarter)
  expect_true(inherits(x, "incidence"))


  # Adding rows that are first date of a year maintains class
  x <-
    inci_year %>%
    slice_head(n = 2) %>%
    mutate(bin_date = as.Date("2020-01-01")) %>%
    bind_rows(inci_year)
  expect_true(inherits(x, "incidence"))


})

test_that("operations that drop class", {

  expect_false(inherits(inci[1:3], "incidence"))


  x <-
    inci %>%
    select(bin_date, count)
  expect_false(inherits(x, "incidence"))

  x <-
    inci %>%
    select(-1)
  expect_false(inherits(x, "incidence"))

  x <-
    inci %>%
    pull(1)

  expect_false(inherits(x, "incidence"))

  x <-
    inci %>%
    transmute(new_count = count + 1)

  expect_false(inherits(x, "incidence"))


  # Changing rows to have wrong interval (e.g. not 2 weeks) drops class
  x <-
    inci %>%
    mutate(
      bin_date = replace(
        bin_date,
        week_group == "2014-W21",
        (bin_date + 3)[week_group == "2014-W21"]
        )
    )

  expect_false(inherits(x, "incidence"))

  # Adding rows with dates that are not multiples of 2 weeks drops class
  x <-
    inci %>%
    slice_head(n = 2) %>%
    mutate(bin_date = bin_date + 30) %>%
    bind_rows(inci)
  expect_false(inherits(x, "incidence"))

  # Adding rows that are not first day of a month drops class
  y <-
    inci_month %>%
    slice_head(n = 2) %>%
    mutate(bin_date = as.Date("2020-03-02"))
    y %>% bind_rows(inci_month)
  expect_false(inherits(x, "incidence"))

  # Adding rows that are not the first date of a quarter drops class
  x <-
    inci_quarter %>%
    slice_head(n = 2) %>%
    mutate(bin_date = as.Date("2020-06-01")) %>%
    bind_rows(inci_quarter)
  expect_false(inherits(x, "incidence"))

  # Adding rows that are not the first date of a year drops class
  x <-
    inci_year %>%
    slice_head(n = 2) %>%
    mutate(bin_date = as.Date("2020-02-01")) %>%
    bind_rows(inci_year)
  expect_false(inherits(x, "incidence"))


  x <- inci
  x[1,1] <- x[1,1] + 3
  expect_false(inherits(x, "incidence"))


})

# TODO - full set of tests for expected behaviour
