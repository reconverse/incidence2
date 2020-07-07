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
    mutate(future = date_group + 999)

  expect_s3_class(x, "incidence")

  x <-
    inci %>%
    rename(left_bin = date_group)

  expect_s3_class(x, "incidence")

  expect_s3_class(inci[], "incidence")


})

test_that("operations that drop class", {

  expect_false(inherits(inci[1:3], "incidence"))


  x <-
    inci %>%
    select(date_group, count)
  expect_false(inherits(x, "incidence"))

  x <-
    inci %>%
    pull(5)

  expect_false(inherits(x, "incidence"))

  x <-
    inci %>%
    transmute(new_count = count + 1)

  expect_false(inherits(x, "incidence"))

  x <- inci
  x <-
    inci %>%
    mutate(
      date_group = replace(
        date_group,
        weeks == "2014-W21",
        (date_group + 3)[weeks == "2014-W21"]
        )
    )

  expect_false(inherits(x, "incidence"))

  x <- inci
  x[1,1] <- x[1,1] + 3
  expect_false(inherits(x, "incidence"))


})

# TODO - full set of tests for expected behaviour
