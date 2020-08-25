context("Checking conversions")

dates <- as.integer(Sys.Date() + 1:10)
x <- incidence(data.frame(dates = dates), date_index = dates)

test_that("as.data.frame works", {
  expected <- data.frame(date = dates, count = 1L)
  expect_identical(as.data.frame(x), expected)
})

test_that("as_tibble works", {
  expected <- tibble::tibble(date = dates, count = 1L)
  expect_identical(as_tibble(x), expected)
})
