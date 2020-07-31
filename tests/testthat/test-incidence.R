context("Incidence main function")

# setting up the data --------------------------------------------------
the_seed <- eval(parse(text = as.character(Sys.Date())))

# Date incidence      --------------------------------------------------
# note: the choice of dates here makes sure first date is 28 Dec 2015, which
# starts an iso week, so that counts will be comparable with/without iso.
# This also ensures that the last date is 2016-04-04 so that there are 15 weeks
# represented here.
set.seed(the_seed)
dat       <- as.integer(c(-3, sample(-3:100, 49, replace = TRUE), 100))
dat_dates <- as.Date("2015-12-31") + dat

test_that("construction - default, integer input", {


  ## USING DAILY INCIDENCE
  x <- incidence(data.frame(dates = dat), date_index = dates)

  ## classes
  expect_is(x, "incidence2")
  expect_is(x$bin_date, class(dat))
  expect_is(x$count, "integer")

  ## results
  expect_false(any(is.na(x$count)))
  expect_equal(nrow(x), diff(range(dat)) + 1)
  expect_equal(sum(x$count), length(dat))
  expect_true(all(diff(x$bin_date) == get_interval(x)))

  ## USING INCIDENCE PER 3 DAYS
  x <- incidence(data.frame(dates = dat), date_index = dates, interval = 3)

  ## String numbers can be interpreted as intervals
  expect_identical(
    x,
    incidence(data.frame(dates = dat), date_index = dates, interval = "3")
  )

  ## classes
  expect_is(x, "incidence2")
  expect_is(x$bin_date, class(dat))
  expect_is(x$count, "integer")

  ## results
  expect_false(any(is.na(x$count)))
  expect_equal(sum(x$count), length(dat))
  expect_true(all(diff(x$bin_date) == get_interval(x)))
})

test_that("construction - ISO week", {


  ## USING WEEKLY INCIDENCE
  inc_week <- incidence(
    data.frame(dates = dat_dates),
    date_index = dates,
    interval = 7,
    standard = FALSE)

  inc_isoweek <- incidence(
    data.frame(dates = dat_dates),
    date_index = dates,
    interval = 7)

  ## classes
  expect_is(inc_week, "incidence2")
  expect_is(inc_isoweek, "incidence2")

  ## results
  expect_false(any(is.na(inc_isoweek$count)))
  expect_equal(sum(inc_isoweek$count), length(dat))
  expect_true(all(diff(inc_isoweek$bin_date) == get_interval(inc_isoweek)))
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

  msg <- paste0("Flooring from non-integer date caused approximations:\n",
                #"Mean relative difference: 0.0228833")
                "Mean relative difference: 0.02288")
  expect_warning(incidence(data.frame(dates = dat_num), date_index = dates),
                 msg)

  x_num <- suppressWarnings(incidence(data.frame(dates = dat_num), date_index = dates))
  x_int <- incidence(data.frame(dates = dat_int), date_index = dates)

  ## compare outputs
  expect_equal(x_num, x_int)
  expect_is(x_num$bin_date, "numeric")
  expect_is(x_int$bin_date, "integer")
})

test_that("construction - Date input", {

  x         <- incidence(data.frame(dates = dat), date_index = dates)
  x_dates   <- incidence(data.frame(dates = dat_dates), date_index = dates)

  expect_message(x_i_trim  <- incidence(data.frame(dates = dat),
                                        date_index = dates,
                                        first_date = 0),
                 "[0-9]+ observations outside of \\[0, [0-9]+\\] were removed."
  )

  expect_message(
    x_d_trim  <- incidence(data.frame(dates = dat_dates),
                           date_index = dates,
                           first_date = "2016-01-01"),
    "[0-9]+ observations outside of \\[2016-01-01, [-0-9]{10}\\] were removed.")


  expect_message({
    expect_failure(expect_warning({
      x_d_trim  <- incidence(data.frame(dates = dat_dates),
                             date_index = dates,
                             first_date = "2016-01-01")
    },
    "options\\(incidence.warn.first_date = FALSE\\)"))
  },
  "[0-9]+ observations outside of \\[2016-01-01, [-0-9]{10}\\] were removed.")


  x_7 <- incidence(data.frame(dates = dat_dates),
                   date_index = dates,
                   interval = 7L,
                   standard = FALSE)

  x_7_iso   <- incidence(data.frame(dates = dat_dates),
                         date_index = dates,
                         interval = "week")

  x_7_week  <- incidence(data.frame(dates = dat_dates),
                            date_index = dates,
                            interval = "week",
                            standard = FALSE)



  ## Here, we can test if starting on a different day gives us expected results
  x_ds <- incidence(data.frame(dates = dat_dates + 1L), date_index = dates)

  x_7_ds <- incidence(data.frame(dates = dat_dates + 1L),
                      date_index = dates,
                      interval = 7L,
                      standard = FALSE)

  x_w_ds <- incidence(data.frame(dates = dat_dates + 1L),
                      date_index = dates,
                      interval = "week",
                      standard = FALSE)

  x_7_ds_iso <- incidence(data.frame(dates = dat_dates + 1L),
                          date_index = dates,
                          interval = 7L)

  x_w_ds_iso <- incidence(data.frame(dates = dat_dates + 1L),
                          date_index = dates,
                          interval = "week")

  ## Testing monthly input
  w <- "The first_date \\(2015-11-30\\) represents a day that does not occur in all months."
  w <- gsub(" ", "\\\\s", w)
  expect_warning(x_mo_no <- incidence(data.frame(dates = dat_dates - 28),
                                      date_index = dates,
                                      interval = "month",
                                      standard = FALSE), w)

  x_mo_iso <- incidence(data.frame(dates = dat_dates),
                                   date_index = dates,
                                   interval = "month")

  expect_equal(
    format(x_mo_iso$bin_date, "%m"),
    unique(format(sort(dat_dates), "%m")))

  expect_equal(
    format(x_mo_iso$bin_date, "%d"),
    rep("01", 5)) # all starts on first

  expect_equal(x_mo_iso$bin_date[[1]], as.Date("2015-12-01"))

  expect_equal(sum(x_mo_iso$count), 51L)

  x_mo <- incidence(data.frame(dates = dat_dates),
                    date_index = dates,
                    interval = "month",
                    standard = FALSE)

  expect_equal(
    format(x_mo$bin_date, "%m"),
    unique(format(sort(dat_dates), "%m"))[-5])

  expect_equal(format(x_mo$bin_date, "%d"), rep("28", 4)) # all starts on the 28th

  expect_equal(x_mo$bin_date[[1]], as.Date("2015-12-28"))

  expect_equal(sum(x_mo$count), 51L)

  ## Testing quarterly input
  w <- "The first_date \\(2015-11-30\\) represents a day that does not occur in all months."
  w <- gsub(" ", "\\\\s", w)
  expect_warning(
    x_qu_no <- incidence(data.frame(dates = dat_dates - 28),
                         date_index = dates,
                         interval = "quarter",
                         standard = FALSE),
    w)

  x_qu_iso <- incidence(data.frame(dates = dat_dates),
                        date_index = dates,
                        interval = "quarter")

  expect_equal(x_qu_iso$bin_date,
               as.Date(c("2015-10-01", "2016-01-01", "2016-04-01")))

  expect_equal(sum(x_qu_iso$count), 51L)

  x_qu <- incidence(data.frame(dates = dat_dates),
                    date_index = dates,
                    interval = "quarter",
                    standard = FALSE)

  expect_equal(x_qu$bin_date, as.Date(c("2015-12-28", "2016-03-28")))

  expect_equal(sum(x_qu$count), 51L)

  ## Testing yearly input
  dat_yr <- c(dat_dates,
              sample(dat_dates + 366, replace = TRUE),
              sample(dat_dates + 366 + 365, replace = TRUE)
  )
  x_yr_iso <- incidence(data.frame(dates = dat_yr),
                        date_index = dates,
                        interval = "year")

  x_yr     <- incidence(data.frame(dates = dat_yr),
                        date_index = dates,
                        interval = "year",
                        standard = FALSE)

  w <- "The first_date \\(2016-02-29\\) represents a day that does not occur in all years."
  w <- gsub(" ", "\\\\s", w)
  expect_warning(
    x_yr_no  <- incidence(data.frame(dates = dat_yr),
                          date_index = dates,
                          interval = "year",
                          first_date = as.Date("2016-02-29"),
                          standard = FALSE),
    w)

  expect_equal(
    x_yr_iso$bin_date,
    as.Date(c("2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01")))

  expect_equal(x_yr$bin_date,
               as.Date(c("2015-12-28", "2016-12-28", "2017-12-28")))

  expect_equal(sum(x_yr$count), sum(x_yr_iso$count))

  ## compare outputs
  expect_equal(x$count, x_dates$count)
  expect_is(x$bin_date, "integer")
  expect_is(x_dates$bin_date, "Date")
  expect_equal(x_7$count, x_7_iso$count)
  expect_equal(x_7_iso$bin_date, x_7_week$bin_date)

  # shifting days gives the desired effect
  expect_equal(x_ds$bin_date[[1]], x_7_ds$bin_date[[1]])
  # TODO - check if we want interval = 7 to be treated like week or not
  #expect_equal(x_ds$bin_date[[1]] - 1L, x_7_ds_iso$bin_date[[1]])
  #expect_identical(x_7_ds_iso$bin_date, x_w_ds_iso$bin_date)
  expect_failure({
    expect_identical(x_w_ds$bin_date, x_w_ds_iso$bin_date)
  })

  ## Printing will be different with text-based interval
  expect_output(print(x_7), "\\interval: 7 days")
  expect_output(print(x_7_iso), "\\interval: 1 week")
})

test_that("construction - POSIXct input", {

  ## USING DAILY INCIDENCE
  dat_pos <- as.POSIXct(dat_dates)
  x_dates <- incidence(data.frame(dates = dat_dates), date_index = dates)
  x_pos <- incidence(data.frame(dates = dat_pos), date_index = dates)

  ## compare outputs
  expect_equal(x_dates$count, x_pos$count)
  expect_is(x_dates$bin_date, "Date")
  expect_is(x_pos$bin_date, "POSIXct")
})

test_that("construction - character input", {
  dats <- Sys.Date() + sample(-100:100, 5)
  datc <- as.character(dats)

  i_date <- incidence(data.frame(dates = dats), date_index = dates)
  i_char <- incidence(data.frame(dates = datc, stringsAsFactors = FALSE),
                      date_index = dates)
  i_chaw <- incidence(
    data.frame(dates = paste(datc, "   "), stringsAsFactors = FALSE),
    date_index = dates
  )

  expect_message(
    i_cham <- incidence(
      data.frame(dates = c(datc, NA, NA), stringsAsFactors = FALSE),
      date_index = dates),
    "2 missing observations were removed."
  )

  expect_is(i_date, "incidence2")
  expect_identical(i_date, i_char)
  expect_identical(i_date, i_chaw)
  expect_identical(i_date, i_cham)
})


test_that("corner cases", {


  expect_error(incidence(data.frame(dates = integer(0)), date_index = dates),
               "At least one \\(non-NA\\) date must be provided")

  expect_error(incidence(data.frame(dates = numeric(0)), date_index = dates),
               "At least one \\(non-NA\\) date must be provided")

  expect_error(incidence(data.frame(dates = NA), date_index = dates),
               "At least one \\(non-NA\\) date must be provided")

  expect_error(incidence(data.frame(dates = NULL), date_index = dates),
               "dates is NULL")

  expect_error(incidence(data.frame(dates = Inf), date_index = dates),
               "At least one \\(non-NA\\) date must be provided")

  expect_error(incidence(data.frame(dates = 1), date_index = dates, interval = "grind"),
               "The interval 'grind' is not valid. Please supply an integer.")

  expect_error(incidence(data.frame(dates = as.Date(Sys.Date())),
                         date_index = dates,
                         last_date = "core"),
               "last_date \\(core\\) could not be converted to Date. Dates must be in ISO 8601 standard format \\(yyyy-mm-dd\\)")

  expect_error(incidence(data.frame(dates = 1),
                         date_index = dates,
                         interval = "week"),
               "The interval 'week' can only be used for Dates")

  expect_error(incidence(data.frame(dates = as.Date(Sys.Date())),
                         date_index = dates,
                         standard = "TRUE"),
               "The argument `standard` must be either `TRUE` or `FALSE`")

  expect_error(incidence(data.frame(dates = sample(10)),
                         date_index = dates,
                         intrval = 2),
               class = "rlib_error_dots_nonempty")

  expect_error(incidence(data.frame(dates = 1),
                         date_index = dates,
                         were = "wolf"),
               class = "rlib_error_dots_nonempty")


  expect_warning(
    incidence(data.frame(dates = c(dat_dates, as.Date("1900-01-01"))),
              date_index = dates),
    "greater than 18262 days \\[1900-01-01 to")

  msg <- 'Not all dates are in ISO 8601 standard format \\(yyyy-mm-dd\\). The first incorrect date is'
  expect_error(
    incidence(data.frame(dates = 'daldkadl', stringsAsFactors = FALSE),
              date_index = dates),
    paste(msg, "daldkadl"))

  dats <- as.character(Sys.Date() + sample(-10:10, 5))
  dats[3] <- "1Q84-04-15"
  expect_error(incidence(data.frame(dates = dats, stringsAsFactors = FALSE),
                         date_index = dates),
               paste(msg, "1Q84-04-15"))

  dats[3] <- "2018-69-11"
  expect_error(incidence(data.frame(dates = dats, stringsAsFactors = FALSE),
                         date_index = dates),
               paste(msg, "2018-69-11"))

  dats[3] <- "01-01-11"
  expect_error(incidence(data.frame(dates = dats, stringsAsFactors = FALSE),
                         date_index = dates),
               paste(msg, "01-01-11"))

  dats[3] <- "01-Apr-11"
  expect_error(incidence(data.frame(dates = dats, stringsAsFactors = FALSE),
                         date_index = dates),
               paste(msg, "01-Apr-11"))

  msg <- paste0("Input could not be converted to date. Accepted formats are:\n",
                "Date, POSIXct, integer, numeric, character")
  expect_error(incidence(data.frame(dates = factor("2001-01-01")),
                         date_index = dates),
                msg)
})

test_that("incidence constructor can handle missing data", {
  miss_dat <- dat
  miss_dat[5] <- NA
  expect_message(incidence(data.frame(dates = miss_dat), date_index = dates),
                           "1 missing observations were removed.")
})

test_that("incidence constructor can handle data out of range with groups", {
 set.seed(the_seed)
 g <- sample(letters[1:2], length(dat), replace = TRUE)
 expect_message(incidence(data.frame(dates = dat, groups = g),
                          date_index = dates,
                          first_date = 0,
                          groups = groups),
                "[0-9]+ observations outside of \\[0, [0-9]+\\] were removed."
 )
})

test_that("Expected values, no group", {


  expect_true(
    all(incidence(data.frame(dates = 1:10), date_index = dates)$count == 1L))

  expect_true(
    all(incidence(data.frame(dates = sample(1:10)), date_index = dates)$count == 1L))

  set.seed(1)

  dat <- data.frame(dates = c(3,2,-1,1,1))
  res1 <- incidence(dat, date_index = dates)
  expect_known_value(res1, file = "rds/incidence.res1.rds")

  dat <- data.frame(dates = c(0,0,0))
  res2 <- incidence(dat, date_index = dates)
  expect_known_value(res2, file = "rds/incidence.res2.rds")

  dat <- data.frame(dates = sample(1:80, 1000, replace = TRUE))
  res3 <- incidence(dat, date_index = dates)
  expect_known_value(res3, file = "rds/incidence.res3.rds")

  dat <- data.frame(dates = as.Date("1984-01-01") + sample(1:100, 200, replace = TRUE))
  res4 <- incidence(dat, date_index = dates)
  expect_known_value(res4, file = "rds/incidence.res4.rds")

  dat <- data.frame(dates = c(3, 2, -1, 1, 1))
  res5 <- incidence(dat, date_index = dates, interval = 2L)
  expect_known_value(res5, file = "rds/incidence.res5.rds")

  dat <- data.frame(dates = c(0,0,0))
  res6 <- incidence(dat, date_index = dates, interval = 3L)
  expect_known_value(res6, file = "rds/incidence.res6.rds")

})


test_that("na_as_group", {
  dat <- data.frame(
      date = Sys.Date() + 1:10,
      names = c(NA, paste("group", 2:9, sep = "_"), NA)
  )

  x <- incidence(dat, date_index = date, groups = names, na_as_group = FALSE)
  expect_true(all(dat$bin_date %in% (Sys.Date() + 2:9)))
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
  expect_known_value(res.g.1, file = "rds/res.g.1.rds")

  dat <- data.frame(dates = dates[[2]], groups = factors[[2]])
  res.g.2 <- incidence(dat, date_index = dates, groups = groups)
  expect_known_value(res.g.2, file = "rds/res.g.2.rds")

  dat <- data.frame(dates = dates[[3]], groups = factors[[3]])
  res.g.3 <- incidence(dat, date_index = dates, groups = groups)
  expect_known_value(res.g.3, file = "rds/res.g.3.rds")

  dates <- as.Date(c("2020-07-30", "2020-07-30", rep("2020-08-06", 3)))
  group1 <- c("Bob", "Bob", "Bob", "George", "George")
  group2 <- c("Cat", "Cat", "Dog", "Dog", "Mouse")
  dat <- data.frame(dates, group1, group2)
  res.g.4 <- incidence(dat,
                       date_index = dates,
                       groups = c(group1, group2),
                       interval = "week")
  expect_known_value(res.g.4, file = "rds/res.g.4.rds")
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

  expect_known_output(print(x), file = "rds/print1.rds")

  expect_known_output(summary(x), file = "rds/summary1.rds")

  dat <- data.frame(dates = 1:2, groups = factor(1:2))
  y <- incidence(dat,date_index = dates, groups = groups)

  expect_known_output(print(y), file = "rds/print2.rds")

  expect_known_output(summary(y), file = "rds/summary2.rds")

})

