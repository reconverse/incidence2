# setting up the data --------------------------------------------------
set.seed(999)

test_that("construction - default, integer input", {

  dat <- 0:104

  # daily incidence
  x <- incidence(data.frame(dates = dat), date_index = dates)

  # classes
  expect_s3_class(x, "incidence2")
  expect_type(x$date, "integer")
  expect_type(x$count, "integer")

  # results
  expect_false(any(is.na(x$count)))
  expect_equal(nrow(x), length(unique(dat)))
  expect_equal(get_timespan(x), diff(range(dat)) + 1)
  expect_equal(sum(x$count), length(dat))
  expect_true(all(diff(x$date) == get_interval(x)))

  # using incidence per 3 days
  x <- incidence(data.frame(dates = dat), date_index = dates, interval = 3)

  # classes
  expect_s3_class(x, "incidence2")
  expect_type(x$date_index, "integer")
  expect_type(x$count, "integer")

  # results
  expect_false(any(is.na(x$count)))
  expect_equal(nrow(x), length(unique(dat)) / 3)
  expect_equal(get_timespan(x), diff(range(dat)) + 1)
  expect_equal(sum(x$count), length(dat))
  expect_true(all(diff(as.integer(x$date_index)) == get_interval(x)))

})

test_that("construction - ISO week", {

  # note: the choice of dates here makes sure first date is 28 Dec 2015, which
  # starts an iso week, so that counts will be comparable with/without iso.
  # This also ensures that the last date is 2016-04-10 so that there are 15 weeks
  # represented here.
  dat <- 0:104
  dat_dates <- as.Date("2015-12-28") + dat

  # weekly incidence
  inc_week <- incidence(
    data.frame(dates = dat_dates),
    date_index = dates,
    interval = "week"
  )

  # iso incidence
  inc_isoweek <- incidence(
    data.frame(dates = dat_dates),
    date_index = dates,
    interval = "isoweek")

  # should be identical results
  expect_identical(inc_week, inc_isoweek)

  # classes
  expect_s3_class(inc_week, "incidence2")
  expect_s3_class(inc_week$date_index, "yrwk")
  expect_type(inc_week$count, "integer")

  # results
  expect_false(any(is.na(inc_week$count)))
  expect_equal(nrow(inc_week), length(unique(dat_dates)) / 7)
  expect_equal(get_timespan(inc_week), as.integer(diff(range(dat_dates)) + 1))
  expect_equal(sum(inc_week$count), length(dat_dates))
  expect_true(all(diff(as.integer(inc_week$date_index)) == 7L))
})


test_that("construction - numeric input", {

  ## USING DAILY INCIDENCE
  dat_int <- c(0L, 2L, 5L, 9L, -1L, 9L, 10L, 6L, 5L, -3L, -1L, -1L, 6L, 2L, 7L,
               3L, 7L, 10L, 2L, 7L, 10L, -1L, 6L, -2L, 0L, 2L, -3L, 2L, 9L, 1L,
               3L, 5L, 3L, -1L, 8L, 6L, 8L, -2L, 7L, 2L, 8L, 6L, 7L, 4L, 4L,
               8L, -3L, 3L, 7L, 6L, 3L, 9L, 3L, 0L, -3L, -2L, 1L, 4L, 6L, 2L,
               9L, 1L, 3L, 1L, 6L, 0L, 3L, 7L, -2L, 9L, 1L, 8L, 1L, 1L, 3L, 9L,
               9L, 2L, 7L, 10L, 3L, 6L, 2L, 1L, 7L, -1L, 6L, -2L, 0L, -1L, 0L,
               -3L, 5L, 9L, 7L, 8L, 3L, 2L, 8L, 5L)

  dat_num <- dat_int + 0.1

  expect_error(
    incidence(data.frame(dates = dat_num), date_index = dates),
    "Where numeric, x[[date_index]] must be a vector of whole numbers",
    fixed = TRUE
  )
})

test_that("construction - Date input", {

  # note: the choice of dates here makes sure first date is 28 Dec 2015, which
  # starts an iso week, so that counts will be comparable with/without iso.
  # This also ensures that the last date is 2016-04-10 so that there are 15 weeks
  # represented here.
  dat <- 0:104
  dat_dates <- as.Date("2015-12-28") + dat

  x <- incidence(data.frame(dates = dat), date_index = dates)

  x_dates <- incidence(data.frame(dates = dat_dates), date_index = dates)

  x_ds <- incidence(
    data.frame(dates = dat_dates + 1L),
    date_index = dates
  )

  x_7 <- incidence(
    data.frame(dates = dat_dates),
    date_index = dates,
    interval = 7L
  )

  x_7_ds <- incidence(
    data.frame(dates = dat_dates + 1L),
    date_index = dates,
    interval = 7L
  )

  x_week  <- incidence(
    data.frame(dates = dat_dates),
    date_index = dates,
    interval = "week"
  )

  x_w_ds <- incidence(
    data.frame(dates = dat_dates + 1L),
    date_index = dates,
    interval = "week",
  )

  x_mo <- incidence(
    data.frame(dates = dat_dates),
    date_index = dates,
    interval = "month"
  )

  x_qu <- incidence(
    data.frame(dates = dat_dates),
    date_index = dates,
    interval = "quarter"
  )

  # check firstdate filter gives message
  expect_message(
    incidence(
      data.frame(dates = dat),
      date_index = dates,
      firstdate = 10
    ),
    "10 observations were removed",
    fixed = TRUE
  )

  expect_message(
    incidence(
      data.frame(dates = dat_dates),
      date_index = dates,
      firstdate = as.Date("2016-01-01")
    ),
    "4 observations were removed",
    fixed = TRUE
  )

  # Testing monthly input
  expect_equal(
    get_month(x_mo$date_index),
    as.integer(unique(format(sort(dat_dates), "%m")))
  )

  expect_equal(as.Date(x_mo$date_index[[1]]), as.Date("2015-12-01"))

  expect_equal(sum(x_mo$count), 105L)

  # Testing quarterly input
  expect_equal(
    as.Date(x_qu$date_index),
    as.Date(c("2015-10-01", "2016-01-01", "2016-04-01"))
  )

  expect_equal(sum(x_qu$count), 105L)

  # Testing yearly input
  dat_yr <- c(
    dat_dates,
    sample(dat_dates + 366, replace = TRUE),
    sample(dat_dates + 366 + 365, replace = TRUE)
  )

  x_yr <- incidence(
    data.frame(dates = dat_yr),
    date_index = dates,
    interval = "year"
  )

  expect_equal(unclass(x_yr$date_index), c(2015, 2016, 2017, 2018))

  expect_equal(
    as.Date(x_yr$date_index),
    as.Date(c("2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01"))
  )

  expect_equal(sum(x_yr$count), 315L)

  # compare outputs
  expect_equal(x$count, x_dates$count)
  expect_equal(x_7$count, x_week$count)
  expect_equal(as.Date(x_7$date_index), as.Date(x_week$date_index))

  # shifting days gives the desired effect
  expect_equal(as.Date(x_ds$date[[1]]), as.Date(x_7_ds$date_index[[1]]))

  # Printing will be different with text-based interval
  expect_output(print(x_7), "interval: 7")
  expect_output(print(x_week), "interval: 1 week")
})


test_that("incidence constructor can handle missing data", {
  dat <- 0:10
  miss_dat <- dat
  miss_dat[5] <- NA
  expect_message(
    incidence(data.frame(dates = miss_dat), date_index = dates),
    "1 missing observations were removed.",
    fixed = TRUE
  )
})

test_that("incidence constructor can handle data out of range with groups", {
  dat <- 0:10
  set.seed(1)
  g <- sample(letters[1:2], length(dat), replace = TRUE)
  expect_message(
    incidence(
      data.frame(dates = dat, groups = g),
      date_index = dates,
      firstdate = 2,
      groups = groups
    ),
    "2 observations were removed",
    fixed = TRUE
  )
})

test_that("Expected values, no group", {
  set.seed(1)

  expect_true(
    all(incidence(data.frame(dates = 1:10), date_index = dates)$count == 1L))

  dat <- data.frame(dates = c(3,2,-1,1,1))
  res1 <- incidence(dat, date_index = dates)
  expect_snapshot_output(res1)

  dat <- data.frame(dates = c(0,0,0))
  res2 <- incidence(dat, date_index = dates)
  expect_snapshot_output(res2)

  dat <- data.frame(dates = sample(1:80, 1000, replace = TRUE))
  res3 <- incidence(dat, date_index = dates)
  expect_snapshot_value(res3, style = "serialize")

  dat <- data.frame(dates = as.Date("1984-01-01") + sample(1:100, 200, replace = TRUE))
  res4 <- incidence(dat, date_index = dates)
  expect_snapshot_value(res4, style = "serialize")

  dat <- data.frame(dates = c(3, 2, -1, 1, 1))
  res5 <- incidence(dat, date_index = dates, interval = 2L)
  expect_snapshot_output(res5)

  dat <- data.frame(dates = c(0,0,0))
  res6 <- incidence(dat, date_index = dates, interval = 3L)
  expect_snapshot_output(res6)
})


test_that("na_as_group", {
  dat <- data.frame(
      date = Sys.Date() + 1:10,
      names = c(NA, paste("group", 2:9, sep = "_"), NA)
  )

  x <- incidence(dat, date_index = date, groups = names, na_as_group = FALSE)
  expect_true(all(dat$date_index %in% (Sys.Date() + 2:9)))
  expect_equal(get_n(x), 8)
})



test_that("Expected values, with groups", {

  dates <- list(
    as.integer(c(3,2,-1,1,1)),
    as.integer(c(0,0,0)),
    as.integer(c(0,1,2,2,3))
  )

  factors <- list(
    factor(c(1,1,2,2,2)),
    factor(c('a','b','a')),
    factor(c(1, 2, 3, 3, 3))
  )

  dat <- data.frame(dates = dates[[1]], groups = factors[[1]])
  res.g.1 <- incidence(dat, date_index = dates, groups = groups)
  expect_snapshot_output(res.g.1)

  dat <- data.frame(dates = dates[[2]], groups = factors[[2]])
  res.g.2 <- incidence(dat, date_index = dates, groups = groups)
  expect_snapshot_output(res.g.2)

  dat <- data.frame(dates = dates[[3]], groups = factors[[3]])
  res.g.3 <- incidence(dat, date_index = dates, groups = groups)
  expect_snapshot_output(res.g.3)

  dates <- as.Date(c("2020-07-30", "2020-07-30", rep("2020-08-06", 3)))
  group1 <- c("Bob", "Bob", "Bob", "George", "George")
  group2 <- c("Cat", "Cat", "Dog", "Dog", "Mouse")
  dat <- data.frame(dates, group1, group2)
  res.g.4 <- incidence(dat,
                       date_index = dates,
                       groups = c(group1, group2),
                       interval = "week")
  expect_snapshot_output(res.g.4)
})

test_that("user-defined group levels are preserved", {
  g <- sample(LETTERS[1:5], 100, replace = TRUE)
  g <- factor(g, levels = LETTERS[5:1])
  dat <- data.frame(dates = rpois(100, 10), g)
  i <- incidence(dat, date_index = dates, groups = g)
  expect_identical(levels(i[[get_group_names(i)]]), levels(g))
})


test_that("Print and summary returns the object", {
  dat <- data.frame(dates = "2001-01-01")
  x <- incidence(dat, date_index = dates)

  expect_snapshot_output(print(x))

  expect_snapshot_output(summary(x))

  dat <- data.frame(dates = 1:2, groups = factor(1:2))
  y <- incidence(dat,date_index = dates, groups = groups)

  expect_snapshot_output(print(y))

  expect_snapshot_output(summary(y))

})


test_that("cnt variable working as expected", {
  dates <- c("2020-08-24", "2020-08-25", "2020-08-25", "2020-09-03")
  counts <- c(1, 2, 3, 4)
  dat <- data.frame(dates, counts)
  x <- incidence(dat, date_index = dates, count = counts, interval = "week")

  expect_snapshot_output(x)
  expect_equal(as.Date(x$date_index), as.Date(c("2020-08-24", "2020-08-31")))
  expect_equal(x$count, c(6, 4))

  # NA's  should be ignored in sums
  dates <- c("2020-08-24", "2020-08-25", "2020-08-25", "2020-09-03")
  counts <- c(1, 2, NA, 4)
  dat <- data.frame(dates, counts)
  x <- incidence(dat, date_index = dates, count = counts, interval = "week")

  expect_snapshot_output(x)
  expect_equal(as.Date(x$date_index), as.Date(c("2020-08-24", "2020-08-31")))
  expect_equal(x$count, c(3, 4))
})

