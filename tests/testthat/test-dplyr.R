library(dplyr)

data(ebola_sim_clean, package = "outbreaks")
dat <- ebola_sim_clean$linelist

# create incidence object
inci <-
  incidence(
    dat,
    date_index = date_of_onset,
    interval = "2 weeks",
    firstdate = as.Date("2014-05-20"),
    groups = c(hospital, gender),
    na_as_group = TRUE
  )


test_that("operations that preserve class", {
  x <- filter(inci, gender == "f", hospital == "Rokupa Hospital")
  expect_s3_class(x, "incidence2")

  x <-slice_sample(inci, prop = 0.1)
  expect_s3_class(x, "incidence2")

  x <- slice_sample(inci, n = 10)
  expect_s3_class(x, "incidence2")

  x <- slice(inci, 1, 5, 10)
  expect_s3_class(x, "incidence2")

  x <- slice_max(inci, order_by = count, n = 10)
  expect_s3_class(x, "incidence2")

  x <- select(inci, everything())
  expect_s3_class(x, "incidence2")

  x <- mutate(inci, future = date_index + 999)
  expect_s3_class(x, "incidence2")

  x <- rename(inci, left_bin = date_index)
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

  x <- select(inci, date_index, count)
  expect_false(inherits(x, "incidence2"))

  x <- select(inci, -1)
  expect_false(inherits(x, "incidence2"))

  x <- pull(inci, 1)
  expect_false(inherits(x, "incidence2"))

  x <- transmute(inci, new_count = count + 1)
  expect_false(inherits(x, "incidence2"))

  # Adding duplicate rows drops class
  x <-
    inci %>%
    slice_head(n = 1) %>%
    bind_rows(inci)
  expect_false(inherits(x, "incidence2"))
})
