# setup -------------------------------------------------------------------

firstday <- as.Date("2020-01-01") # Wednesday
lastday <- as.Date("2021-12-31")  # Friday
dates <- seq.Date(from = firstday, to = lastday, by = "day")
ht <- c(rep("short", 366), rep("tall", 365))
sz <- c(rep("small", 31), rep("big", 700))
cnt <- c(rep(1L, 366), rep(2L, 365))

dat_dates <- data.frame(date = dates, height = ht, size = sz, count = cnt)

dat_posixct <- data.frame(date = as.POSIXct(dates), height = ht, size = sz, count = cnt)

dat_posixlt <- data.frame(date = as.POSIXlt(dates), height = ht, size = sz, count = cnt)

dat_char <- data.frame(date = as.character(dates), height = ht, size = sz, count = cnt)

dat_int <- data.frame(date = 1:731, height = ht, size = sz, count = cnt)

dat_numeric <- data.frame(date = as.numeric(1:731), height = ht, size = sz, count = cnt)



# day groupings work as expected ------------------------------------------
test_that("single day, no groupings and without count work as expected", {

  x <- incidence(dat_dates, date_index = date)
  x2 <- incidence(dat_dates, date_index = date, interval = "1 days")

  # classes
  expect_s3_class(x, "incidence2")
  expect_s3_class(x$date_index, "Date")
  expect_type(x$count, "integer")

  # results
  expect_equal(nrow(x), 731L)
  expect_true(all(x$date_index == dates))
  expect_true(all(x$count == 1L))
  expect_identical(x$date_index, x2$date_index)
  expect_identical(x$count, x2$count)
})


test_that("single day, no groupings and with count work as expected", {

  x <- incidence(dat_dates, date_index = date, count = count)
  x2 <- incidence(dat_dates, date_index = date, count = count, interval = "day")

  # classes
  expect_s3_class(x, "incidence2")
  expect_s3_class(x$date_index, "Date")

  # results
  expect_equal(nrow(x), 731L)
  expect_true(all(x$date_index == dates))
  expect_equal(x$count, cnt)
  expect_identical(x$date_index, x2$date_index)
  expect_identical(x$count, x2$count)
})


test_that("multi-day, no groupings and without count work as expected", {

  x <- incidence(dat_dates, date_index = date, interval = 17)
  x2 <- incidence(dat_dates, date_index = date, interval = "17 days")
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "17 days")
  expected_counts <- rep(17L, 43)

  # classes
  expect_s3_class(x, "incidence2")
  expect_s3_class(x$date_index, "period")
  expect_type(x$count, "integer")

  # results
  expect_equal(nrow(x), 43L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_identical(as.Date(x$date_index), as.Date(x2$date_index))
  expect_identical(x$count, x2$count)
})


test_that("multi-day, no groupings and with count work as expected", {

  x <- incidence(dat_dates, date_index = date, interval = 17, count = count)
  x2 <- incidence(dat_dates, date_index = date, interval = "17 days", count = count)
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "17 days")
  expected_counts <- c(rep(17L, 21), 25L, rep(34L, 21))

  # classes
  expect_s3_class(x, "incidence2")
  expect_s3_class(x$date_index, "period")
  expect_type(x$count, "integer")

  # results
  expect_equal(nrow(x), 43L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_identical(as.Date(x$date_index), as.Date(x2$date_index))
  expect_identical(x$count, x2$count)
})



# week groupings work as expected -----------------------------------------
test_that("single week, no groupings and without count work as expected", {

  # we use a wednesday week here as that's when the dates start
  x <- incidence(dat_dates, date_index = date, interval = "wednesday week")
  x2 <- incidence(dat_dates, date_index = date, interval = 7)
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "7 days")
  expected_counts <- c(rep(7L, 104), 3L)

  # classes
  expect_s3_class(x, "incidence2")
  expect_s3_class(x$date_index, "yrwk")
  expect_type(x$count, "integer")

  # results
  expect_equal(nrow(x), 105L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_identical(as.Date(x$date_index), as.Date(x2$date_index))
  expect_identical(x$count, x2$count)

})


test_that("single week, no groupings and with count work as expected", {

  # we use a wednesday week here as that's when the dates start
  x <- incidence(dat_dates, date_index = date, interval = "wednesday week", count = count)
  x2 <- incidence(dat_dates, date_index = date, interval = 7, count = count)
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "7 days")
  expected_counts <- c(rep(7L, 52), 12L, rep(14L, 51), 6L)

  # classes
  expect_s3_class(x, "incidence2")
  expect_s3_class(x$date_index, "yrwk")
  expect_type(x$count, "integer")

  # results
  expect_equal(nrow(x), 105L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_identical(as.Date(x$date_index), as.Date(x2$date_index))
  expect_identical(x$count, x2$count)
})


test_that("multi-week, no groupings and without count work as expected", {

  # we use a wednesday week here as that's when the dates start
  x <- incidence(dat_dates, date_index = date, interval = "2 wednesday weeks")
  x2 <- incidence(dat_dates, date_index = date, interval = 14)
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "14 days")
  expected_counts <- c(rep(14L, 52), 3L)

  # classes
  expect_s3_class(x, "incidence2")
  expect_s3_class(x$date_index, "period")
  expect_type(x$count, "integer")

  # results
  expect_equal(nrow(x), 53L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_identical(as.Date(x$date_index), as.Date(x2$date_index))
  expect_identical(x$count, x2$count)
})


test_that("multi-week, no groupings and with count work as expected", {

  # we use a wednesday week here as that's when the dates start
  x <- incidence(dat_dates, date_index = date, interval = "2 wednesday weeks", count = count)
  x2 <- incidence(dat_dates, date_index = date, interval = 14, count = count)
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "14 days")
  expected_counts <- c(rep(14L, 26), 26L, rep(28L, 25), 6L)

  # classes
  expect_s3_class(x, "incidence2")
  expect_s3_class(x$date_index, "period")
  expect_type(x$count, "integer")

  # results
  expect_equal(nrow(x), 53L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_identical(as.Date(x$date_index), as.Date(x2$date_index))
  expect_identical(x$count, x2$count)

})


test_that("week defaults to a monday", {

  # firstday is wednesday => firsday - 2 is monday
  x <- incidence(dat_dates, date_index = date, interval = "week")
  expected_dates <- seq.Date(from = firstday - 2, to = lastday, by = "7 days")
  expected_counts <- c(5L, rep(7L, 103), 5L)

  # classes
  expect_s3_class(x, "incidence2")
  expect_s3_class(x$date_index, "yrwk")
  expect_type(x$count, "integer")

  # results
  expect_equal(nrow(x), 105L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
})



# month groupings work as expected ----------------------------------------
test_that("single month, no groupings and without count work as expected", {

  x <- incidence(dat_dates, date_index = date, interval = "month")
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "1 month")
  expected_counts <- c(
    31L, 29L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L,
    31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L
  )

  # classes
  expect_s3_class(x, "incidence2")
  expect_s3_class(x$date_index, "yrmon")
  expect_type(x$count, "integer")

  # results
  expect_equal(nrow(x), 24L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)

})


test_that("single month, no groupings and with count work as expected", {

  x <- incidence(dat_dates, date_index = date, interval = "month", count = count)
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "1 month", count = count)
  expected_counts <- c(
    31L, 29L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L,
    62L, 56L, 62L, 60L, 62L, 60L, 62L, 62L, 60L, 62L, 60L, 62L
  )

  # classes
  expect_s3_class(x, "incidence2")
  expect_s3_class(x$date_index, "yrmon")
  expect_type(x$count, "integer")

  # results
  expect_equal(nrow(x), 24L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)

})


test_that("multi-month, no groupings and without count work as expected", {

  x <- incidence(dat_dates, date_index = date, interval = "2 month")
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "2 months")
  expected_counts <- c(
    60L, 61L, 61L, 62L, 61L, 61L,
    59L, 61L, 61L, 62L, 61L, 61L
  )

  # classes
  expect_s3_class(x, "incidence2")
  expect_s3_class(x$date_index, "period")
  expect_type(x$count, "integer")

  # results
  expect_equal(nrow(x), 12L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
})


test_that("multi-month, no groupings and with count work as expected", {

  x <- incidence(dat_dates, date_index = date, interval = "2 month", count = count)
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "2 months", count = count)
  expected_counts <- c(
    60L, 61L, 61L, 62L, 61L, 61L,
    118L, 122L, 122L, 124L, 122L, 122L
  )

  # classes
  expect_s3_class(x, "incidence2")
  expect_s3_class(x$date_index, "period")
  expect_type(x$count, "integer")

  # results
  expect_equal(nrow(x), 12L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
})


# quarter groupings work as expected ----------------------------------------
test_that("single quarter, no groupings and without count work as expected", {

  x <- incidence(dat_dates, date_index = date, interval = "quarter")
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "1 quarter")
  expected_counts <- c(
    91L, 91L, 92L, 92L,
    90L, 91L, 92L, 92L
  )

  # classes
  expect_s3_class(x, "incidence2")
  expect_s3_class(x$date_index, "yrqtr")
  expect_type(x$count, "integer")

  # results
  expect_equal(nrow(x), 8L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)

})


test_that("single quarter, no groupings and with count work as expected", {

  x <- incidence(dat_dates, date_index = date, interval = "quarter", count = count)
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "1 quarter", count = count)
  expected_counts <- c(
    91L, 91L, 92L, 92L,
    180L, 182L, 184L, 184L
  )

  # classes
  expect_s3_class(x, "incidence2")
  expect_s3_class(x$date_index, "yrqtr")
  expect_type(x$count, "integer")

  # results
  expect_equal(nrow(x), 8L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)

})


test_that("multi-quarter, no groupings and without count work as expected", {

  x <- incidence(dat_dates, date_index = date, interval = "2 quarter")
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "2 quarters")
  expected_counts <- c(
    182L, 184L,
    181L, 184L
  )

  # classes
  expect_s3_class(x, "incidence2")
  expect_s3_class(x$date_index, "period")
  expect_type(x$count, "integer")

  # results
  expect_equal(nrow(x), 4L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
})


test_that("multi-quarter, no groupings and with count work as expected", {

  x <- incidence(dat_dates, date_index = date, interval = "2 quarter", count = count)
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "2 quarters", count = count)
  expected_counts <- c(
    182L, 184L,
    362L, 368L
  )

  # classes
  expect_s3_class(x, "incidence2")
  expect_s3_class(x$date_index, "period")
  expect_type(x$count, "integer")

  # results
  expect_equal(nrow(x), 4L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
})



# year groupings work as expected ----------------------------------------
test_that("single year, no groupings and without count work as expected", {

  x <- incidence(dat_dates, date_index = date, interval = "year")
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "1 year")
  expected_counts <- c(
    366L,
    365L
  )

  # classes
  expect_s3_class(x, "incidence2")
  expect_s3_class(x$date_index, "yr")
  expect_type(x$count, "integer")

  # results
  expect_equal(nrow(x), 2L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)

})


test_that("single year, no groupings and with count work as expected", {

  x <- incidence(dat_dates, date_index = date, interval = "year", count = count)
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "1 year", count = count)
  expected_counts <- c(
    366,
    730L
  )

  # classes
  expect_s3_class(x, "incidence2")
  expect_s3_class(x$date_index, "yr")
  expect_type(x$count, "integer")

  # results
  expect_equal(nrow(x), 2L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)

})


test_that("multi-year, no groupings and without count work as expected", {

  x <- incidence(dat_dates, date_index = date, interval = "2 year")
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "2 years")
  expected_counts <- 731L

  # classes
  expect_s3_class(x, "incidence2")
  expect_s3_class(x$date_index, "period")
  expect_type(x$count, "integer")

  # results
  expect_equal(nrow(x), 1L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
})


test_that("multi-year, no groupings and with count work as expected", {

  x <- incidence(dat_dates, date_index = date, interval = "2 year", count = count)
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "2 years", count = count)
  expected_counts <- 1096L

  # classes
  expect_s3_class(x, "incidence2")
  expect_s3_class(x$date_index, "period")
  expect_type(x$count, "integer")

  # results
  expect_equal(nrow(x), 1L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
})


# POSIXct groupings work as expected -------------------------------------
test_that("posixct single week, no groupings and with count work as expected", {

  # we use a wednesday week here as that's when the dates start
  x <- incidence(dat_posixct, date_index = date, interval = "wednesday week", count = count)
  x2 <- incidence(dat_posixct, date_index = date, interval = 7, count = count)
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "7 days")
  expected_counts <- c(rep(7L, 52), 12L, rep(14L, 51), 6L)

  # classes
  expect_s3_class(x, "incidence2")
  expect_s3_class(x$date_index, "yrwk")
  expect_type(x$count, "integer")

  # results
  expect_equal(nrow(x), 105L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_identical(as.Date(x$date_index), as.Date(x2$date_index))
  expect_identical(x$count, x2$count)
})



# POSIXlt groupings work as expected -------------------------------------
test_that("posixlt single week, no groupings and with count work as expected", {

  # we use a wednesday week here as that's when the dates start
  x <- incidence(dat_posixlt, date_index = date, interval = "wednesday week", count = count)
  x2 <- incidence(dat_posixlt, date_index = date, interval = 7, count = count)
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "7 days")
  expected_counts <- c(rep(7L, 52), 12L, rep(14L, 51), 6L)

  # classes
  expect_s3_class(x, "incidence2")
  expect_s3_class(x$date_index, "yrwk")
  expect_type(x$count, "integer")

  # results
  expect_equal(nrow(x), 105L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_identical(as.Date(x$date_index), as.Date(x2$date_index))
  expect_identical(x$count, x2$count)
})


# character groupings work as expected ----------------------------------
test_that("character single week, no groupings and with count work as expected", {

  # we use a wednesday week here as that's when the dates start
  x <- incidence(dat_char, date_index = date, interval = "wednesday week", count = count)
  x2 <- incidence(dat_char, date_index = date, interval = 7, count = count)
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "7 days")
  expected_counts <- c(rep(7L, 52), 12L, rep(14L, 51), 6L)

  # classes
  expect_s3_class(x, "incidence2")
  expect_s3_class(x$date_index, "yrwk")
  expect_type(x$count, "integer")

  # results
  expect_equal(nrow(x), 105L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_identical(as.Date(x$date_index), as.Date(x2$date_index))
  expect_identical(x$count, x2$count)
})



# integer dates -----------------------------------------------------------


