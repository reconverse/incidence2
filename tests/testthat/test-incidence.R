# setup -------------------------------------------------------------------

firstday <- as.Date("2020-01-01") # Wednesday
lastday <- as.Date("2021-12-31")  # Friday
dates <- seq.Date(from = firstday, to = lastday, by = "day")
count <- c(rep(1L, 366), rep(2L, 365))

dat_dates <- data.frame(date = dates, count = count)
dat_posixct <- data.frame(date = as.POSIXct(dates), count = count)
dat_posixlt <- data.frame(date = as.POSIXlt(dates), count = count)
dat_char <- data.frame(date = as.character(dates), count = count)
dat_int <- data.frame(date = 1:10, count = c(rep(1, 5), rep(2, 5)))
dat_numeric <- data.frame(date = as.numeric(1:10), count = c(rep(1, 5), rep(2, 5)))



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
  expect_snapshot_output(print(x))
  expect_snapshot_output(summary(x))
})


test_that("single day, no groupings and with count work as expected", {

  x <- incidence(dat_dates, date_index = date, count = count)
  x2 <- incidence(dat_dates, date_index = date, count = count, interval = "day")

  # class
  expect_s3_class(x$date_index, "Date")

  # results
  expect_equal(nrow(x), 731L)
  expect_true(all(x$date_index == dates))
  expect_equal(x$count, count)
  expect_identical(x$date_index, x2$date_index)
  expect_identical(x$count, x2$count)
  expect_snapshot_output(print(x))
  expect_snapshot_output(summary(x))
})


test_that("multi-day, no groupings and without count work as expected", {

  x <- incidence(dat_dates, date_index = date, interval = 17)
  x2 <- incidence(dat_dates, date_index = date, interval = "17 days")
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "17 days")
  expected_counts <- rep(17L, 43)

  # class
  expect_s3_class(x$date_index, "period")

  # results
  expect_equal(nrow(x), 43L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_identical(as.Date(x$date_index), as.Date(x2$date_index))
  expect_identical(x$count, x2$count)
  expect_snapshot_output(print(x))
  expect_snapshot_output(summary(x))
})


test_that("multi-day, no groupings and with count work as expected", {

  x <- incidence(dat_dates, date_index = date, interval = 17, count = count)
  x2 <- incidence(dat_dates, date_index = date, interval = "17 days", count = count)
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "17 days")
  expected_counts <- c(rep(17L, 21), 25L, rep(34L, 21))

  # class
  expect_s3_class(x$date_index, "period")

  # results
  expect_equal(nrow(x), 43L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_identical(as.Date(x$date_index), as.Date(x2$date_index))
  expect_identical(x$count, x2$count)
  expect_snapshot_output(print(x))
  expect_snapshot_output(summary(x))
})



# week groupings work as expected -----------------------------------------
test_that("single week, no groupings and without count work as expected", {

  # we use a wednesday week here as that's when the dates start
  x <- incidence(dat_dates, date_index = date, interval = "wednesday week")
  x2 <- incidence(dat_dates, date_index = date, interval = 7)
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "7 days")
  expected_counts <- c(rep(7L, 104), 3L)

  # class
  expect_s3_class(x$date_index, "yrwk")

  # results
  expect_equal(nrow(x), 105L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_identical(as.Date(x$date_index), as.Date(x2$date_index))
  expect_identical(x$count, x2$count)
  expect_snapshot_output(print(x))
  expect_snapshot_output(summary(x))
})


# week groupings work as expected -----------------------------------------
test_that("single week, with groups and without count work as expected", {

  firstday <- as.Date("2021-02-01") # monday
  lastday <- as.Date("2021-02-28")  # sunday
  dates <- seq.Date(from = firstday, to = lastday, by = "day")
  count <- c(rep(1L, 14), rep(2L, 14))
  height <- c(rep("short", 14), rep("tall", 14))
  size <- c(rep("small", 7), rep("large", 21))
  dat <- data.frame(
    dates = rep(dates, 2),
    height = c(height, rev(height)),
    size = rep(size, 2),
    count = rep(count, 2)
  )

  x <- incidence(dat, date_index = dates, groups = c(height, size), interval = "week")

  expected_dates <- seq.Date(from = firstday, to = lastday, by = "7 days")
  expected_dates <- rep(expected_dates, each = 2)
  expected_counts <- rep(7L, 8)
  expected_heights <- rep(c("short", "tall"), 4)
  expected_sizes <- c(rep("small", 2), rep("large", 6))

  # class
  expect_s3_class(x$date_index, "yrwk")

  # results
  expect_equal(nrow(x), 8L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_equal(x$height, expected_heights)
  expect_equal(x$size, expected_sizes)
  expect_snapshot_output(print(x))
  expect_snapshot_output(summary(x))
})


test_that("single week, with groups and with count work as expected", {

  firstday <- as.Date("2021-02-01") # monday
  lastday <- as.Date("2021-02-28")  # sunday
  dates <- seq.Date(from = firstday, to = lastday, by = "day")
  count <- c(rep(1L, 14), rep(2L, 14))
  height <- c(rep("short", 14), rep("tall", 14))
  size <- c(rep("small", 7), rep("large", 21))
  dat <- data.frame(
    dates = rep(dates, 2),
    height = c(height, rev(height)),
    size = rep(size, 2),
    count = rep(count, 2)
  )

  x <- incidence(dat, date_index = dates, groups = c(height, size), interval = "week", count = count)

  expected_dates <- seq.Date(from = firstday, to = lastday, by = "7 days")
  expected_dates <- rep(expected_dates, each = 2)
  expected_counts <- c(rep(7L, 4), rep(14L, 4))
  expected_heights <- rep(c("short", "tall"), 4)
  expected_sizes <- c(rep("small", 2), rep("large", 6))

  # class
  expect_s3_class(x$date_index, "yrwk")

  # results
  expect_equal(nrow(x), 8L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_equal(x$height, expected_heights)
  expect_equal(x$size, expected_sizes)
  expect_snapshot_output(print(x))
  expect_snapshot_output(summary(x))
})

test_that("single week, no groupings and with count work as expected", {

  # we use a wednesday week here as that's when the dates start
  x <- incidence(dat_dates, date_index = date, interval = "wednesday week", count = count)
  x2 <- incidence(dat_dates, date_index = date, interval = 7, count = count)
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "7 days")
  expected_counts <- c(rep(7L, 52), 12L, rep(14L, 51), 6L)

  # class
  expect_s3_class(x$date_index, "yrwk")

  # results
  expect_equal(nrow(x), 105L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_identical(as.Date(x$date_index), as.Date(x2$date_index))
  expect_identical(x$count, x2$count)
  expect_snapshot_output(print(x))
  expect_snapshot_output(summary(x))
})


test_that("multi-week, no groupings and without count work as expected", {

  # we use a wednesday week here as that's when the dates start
  x <- incidence(dat_dates, date_index = date, interval = "2 wednesday weeks")
  x2 <- incidence(dat_dates, date_index = date, interval = 14)
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "14 days")
  expected_counts <- c(rep(14L, 52), 3L)

  # class
  expect_s3_class(x$date_index, "period")

  # results
  expect_equal(nrow(x), 53L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_identical(as.Date(x$date_index), as.Date(x2$date_index))
  expect_identical(x$count, x2$count)
  expect_snapshot_output(print(x))
  expect_snapshot_output(summary(x))
})


test_that("multi-week, no groupings and with count work as expected", {

  # we use a wednesday week here as that's when the dates start
  x <- incidence(dat_dates, date_index = date, interval = "2 wednesday weeks", count = count)
  x2 <- incidence(dat_dates, date_index = date, interval = 14, count = count)
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "14 days")
  expected_counts <- c(rep(14L, 26), 26L, rep(28L, 25), 6L)

  # class
  expect_s3_class(x$date_index, "period")

  # results
  expect_equal(nrow(x), 53L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_identical(as.Date(x$date_index), as.Date(x2$date_index))
  expect_identical(x$count, x2$count)
  expect_snapshot_output(print(x))
  expect_snapshot_output(summary(x))
})


test_that("week defaults to a monday", {

  # firstday is wednesday => firsday - 2 is monday
  x <- incidence(dat_dates, date_index = date, interval = "week")
  expected_dates <- seq.Date(from = firstday - 2, to = lastday, by = "7 days")
  expected_counts <- c(5L, rep(7L, 103), 5L)

  # class
  expect_s3_class(x$date_index, "yrwk")

  # results
  expect_equal(nrow(x), 105L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_snapshot_output(print(x))
  expect_snapshot_output(summary(x))
})



# month groupings work as expected ----------------------------------------
test_that("single month, no groupings and without count work as expected", {

  x <- incidence(dat_dates, date_index = date, interval = "month")
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "1 month")
  expected_counts <- c(
    31L, 29L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L,
    31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L
  )

  # class
  expect_s3_class(x$date_index, "yrmon")

  # results
  expect_equal(nrow(x), 24L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_snapshot_output(print(x))
  expect_snapshot_output(summary(x))
})


test_that("single month, no groupings and with count work as expected", {

  x <- incidence(dat_dates, date_index = date, interval = "month", count = count)
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "1 month", count = count)
  expected_counts <- c(
    31L, 29L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L,
    62L, 56L, 62L, 60L, 62L, 60L, 62L, 62L, 60L, 62L, 60L, 62L
  )

  # class
  expect_s3_class(x$date_index, "yrmon")

  # results
  expect_equal(nrow(x), 24L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_snapshot_output(print(x))
  expect_snapshot_output(summary(x))
})


test_that("multi-month, no groupings and without count work as expected", {

  x <- incidence(dat_dates, date_index = date, interval = "2 month")
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "2 months")
  expected_counts <- c(
    60L, 61L, 61L, 62L, 61L, 61L,
    59L, 61L, 61L, 62L, 61L, 61L
  )

  # class
  expect_s3_class(x$date_index, "period")

  # results
  expect_equal(nrow(x), 12L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_snapshot_output(print(x))
  expect_snapshot_output(summary(x))
})


test_that("multi-month, no groupings and with count work as expected", {

  x <- incidence(dat_dates, date_index = date, interval = "2 month", count = count)
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "2 months", count = count)
  expected_counts <- c(
    60L, 61L, 61L, 62L, 61L, 61L,
    118L, 122L, 122L, 124L, 122L, 122L
  )

  # class
  expect_s3_class(x$date_index, "period")

  # results
  expect_equal(nrow(x), 12L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_snapshot_output(print(x))
  expect_snapshot_output(summary(x))
})


# quarter groupings work as expected ----------------------------------------
test_that("single quarter, no groupings and without count work as expected", {

  x <- incidence(dat_dates, date_index = date, interval = "quarter")
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "1 quarter")
  expected_counts <- c(
    91L, 91L, 92L, 92L,
    90L, 91L, 92L, 92L
  )

  # class
  expect_s3_class(x$date_index, "yrqtr")

  # results
  expect_equal(nrow(x), 8L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_snapshot_output(print(x))
  expect_snapshot_output(summary(x))
})


test_that("single quarter, no groupings and with count work as expected", {

  x <- incidence(dat_dates, date_index = date, interval = "quarter", count = count)
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "1 quarter", count = count)
  expected_counts <- c(
    91L, 91L, 92L, 92L,
    180L, 182L, 184L, 184L
  )

  # class
  expect_s3_class(x$date_index, "yrqtr")

  # results
  expect_equal(nrow(x), 8L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_snapshot_output(print(x))
  expect_snapshot_output(summary(x))
})


test_that("multi-quarter, no groupings and without count work as expected", {

  x <- incidence(dat_dates, date_index = date, interval = "2 quarter")
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "2 quarters")
  expected_counts <- c(
    182L, 184L,
    181L, 184L
  )

  # class
  expect_s3_class(x$date_index, "period")

  # results
  expect_equal(nrow(x), 4L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_snapshot_output(print(x))
  expect_snapshot_output(summary(x))
})


test_that("multi-quarter, no groupings and with count work as expected", {

  x <- incidence(dat_dates, date_index = date, interval = "2 quarter", count = count)
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "2 quarters", count = count)
  expected_counts <- c(
    182L, 184L,
    362L, 368L
  )

  # class
  expect_s3_class(x$date_index, "period")

  # results
  expect_equal(nrow(x), 4L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_snapshot_output(print(x))
  expect_snapshot_output(summary(x))
})



# year groupings work as expected ----------------------------------------
test_that("single year, no groupings and without count work as expected", {

  x <- incidence(dat_dates, date_index = date, interval = "year")
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "1 year")
  expected_counts <- c(
    366L,
    365L
  )

  # class
  expect_s3_class(x$date_index, "yr")

  # results
  expect_equal(nrow(x), 2L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_snapshot_output(print(x))
  expect_snapshot_output(summary(x))
})


test_that("single year, no groupings and with count work as expected", {

  x <- incidence(dat_dates, date_index = date, interval = "year", count = count)
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "1 year", count = count)
  expected_counts <- c(
    366,
    730L
  )

  # class
  expect_s3_class(x$date_index, "yr")

  # results
  expect_equal(nrow(x), 2L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_snapshot_output(print(x))
  expect_snapshot_output(summary(x))
})


test_that("multi-year, no groupings and without count work as expected", {

  x <- incidence(dat_dates, date_index = date, interval = "2 year")
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "2 years")
  expected_counts <- 731L

  # class
  expect_s3_class(x$date_index, "period")

  # results
  expect_equal(nrow(x), 1L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_snapshot_output(print(x))
  expect_snapshot_output(summary(x))
})


test_that("multi-year, no groupings and with count work as expected", {

  x <- incidence(dat_dates, date_index = date, interval = "2 year", count = count)
  expected_dates <- seq.Date(from = firstday, to = lastday, by = "2 years", count = count)
  expected_counts <- 1096L

  # class
  expect_s3_class(x$date_index, "period")

  # results
  expect_equal(nrow(x), 1L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$count, expected_counts)
  expect_snapshot_output(print(x))
  expect_snapshot_output(summary(x))
})


# POSIXct groupings work as expected -------------------------------------
test_that("posixct single week, no groupings and with count work as expected", {

  x <- incidence(dat_posixct, date_index = date, interval = "wednesday week", count = count)
  x2 <- incidence(dat_dates, date_index = date, interval = "wednesday week", count = count)
  expect_identical(x, x2)
})



# POSIXlt groupings work as expected -------------------------------------
test_that("posixlt single week, no groupings and with count work as expected", {

  x <- incidence(dat_posixlt, date_index = date, interval = "wednesday week", count = count)
  x2 <- incidence(dat_dates, date_index = date, interval = "wednesday week", count = count)
  expect_identical(x, x2)
})


# character groupings work as expected ----------------------------------
test_that("character single week, no groupings and with count work as expected", {

  x <- incidence(dat_char, date_index = date, interval = "wednesday week", count = count)
  x2 <- incidence(dat_dates, date_index = date, interval = "wednesday week", count = count)
  expect_identical(x, x2)
})



# integer dates -----------------------------------------------------------
test_that("integer date periods without counts work as expected", {
  x <- incidence(dat_int, date_index = date, interval = 5)

  # class
  expect_s3_class(x$date_index, "int_period")

  # results
  expect_equal(nrow(x), 2L)
  expect_equal(as.integer(x$date_index), c(1L, 6L))
  expect_equal(x$count, c(5L, 5L))
  expect_snapshot_output(print(x))
  expect_snapshot_output(summary(x))
})


test_that("integer date periods with counts work as expected", {
  x <- incidence(dat_int, date_index = date, interval = 5, count = count)

  # class
  expect_s3_class(x$date_index, "int_period")

  # results
  expect_equal(nrow(x), 2L)
  expect_equal(as.integer(x$date_index), c(1L, 6L))
  expect_equal(x$count, c(5L, 10L))
  expect_snapshot_output(print(x))
  expect_snapshot_output(summary(x))
})


test_that("numeric date periods work as expected", {
  x <- incidence(dat_int, date_index = date, interval = 5, count = count)
  xx <- incidence(dat_numeric, date_index = date, interval = 5, count = count)
  expect_identical(x, xx)
})


test_that("date_index works for multiple values", {
  firstday <- as.Date("2021-01-01")
  lastday <- as.Date("2021-12-31")
  dates_1 <- seq.Date(from = firstday, to = lastday, by = "day")
  dates_2 <- dates_1 - 31
  dat <- data.frame(dates_1, dates_2)
  x <- incidence(
    dat,
    date_index = c(deaths = dates_1, onset = dates_2),
    interval = "month"
  )

  # not using named date_index when of length > 1 is an error
  expect_error(incidence(dat, date_index = c(dates_1, dates_2)))

  expected_dates <- seq(as.Date("2020-12-01"), as.Date("2021-12-01"), "month")
  expected_deaths <- c(0, 31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
  expected_onsets <- c(31L, 31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 0L)
  expect_equal(as.Date(x$date_index), expected_dates)
  expect_equal(x$deaths, expected_deaths)
  expect_equal(x$onset, expected_onsets)
})
