library(dplyr)

data(ebola_sim_clean, package = "outbreaks")
dat <- ebola_sim_clean$linelist

# create incidence object
inci <-
  dat %>%
  incidence(
    date_index = date_of_onset,
    interval = "2 weeks",
    firstdate = as.Date("2014-05-20"),
    groups = c(hospital, gender),
    na_as_group = TRUE
  )

inci_month <-
  dat %>%
  incidence(
    date_index = date_of_onset,
    interval = "month",
    firstdate = as.Date("2014-05-20"),
    groups = c(hospital, gender),
    na_as_group = TRUE
  )


inci_quarter <-
  dat %>%
  incidence(
    date_index = date_of_onset,
    interval = "quarter",
    firstdate = as.Date("2014-05-20"),
    groups = c(hospital, gender),
    na_as_group = TRUE
  )

inci_year <-
  dat %>%
  incidence(
    date_index = date_of_onset,
    interval = "year",
    firstdate = as.Date("2014-05-20"),
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
})
