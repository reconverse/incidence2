context("Checking conversions")

dates <- as.integer(Sys.Date() + 1:10)
x <- incidence(data.frame(dates = dates), date_index = dates)

test_that("as.data.frame works", {
  expected <- data.frame(bin_date = dates, count = 1L)
  expect_identical(as.data.frame(x), expected)
})

test_that("as_tibble works", {
  expected <- tibble::tibble(bin_date = dates, count = 1L)
  expect_identical(as_tibble(x), expected)
})



test_that("as_incidence works", {
  data(ebola_sim_clean, package = "outbreaks")
  dat <- ebola_sim_clean$linelist
  inci <- incidence(
    dat,
    date_index = date_of_onset,
    interval = "2 weeks",
    first_date = "2014-05-20",
    last_date = "2014-06-10",
    groups = c(hospital, gender),
    na_as_group = TRUE
  )
  dat2 <- as.data.frame(inci)

  expect_identical(
    as_incidence(dat2, bin_date, count, c(hospital, gender),
                 interval = "2 weeks"),
    inci
    )

  dat <- data.frame(
    dates = c(1,2,3,3),
    count = c(4,2,3,3)
  )
  expect_error(as_incidence(dat, dates, count),
               "Cannot convert a dataframe with duplicated rows into an incidence object",
               fixed = TRUE)
})




