test_that("incidence with no groupings and no intervals works", {
    firstday <- as.Date("2020-01-01") # Wednesday
    lastday <- as.Date("2021-12-31")  # Friday
    dates <- seq.Date(from = firstday, to = lastday, by = "day")
    count <- c(rep(1L, 366), rep(2L, 365))
    dat <- data.frame(date = c(dates,dates), count = c(count, count))

    # no groupings and no counts
    x <- incidence(dat, date_index = "date")
    expect_s3_class(x, c("incidence2", "data.frame"), exact = TRUE)
    expect_equal(nrow(x), 731L)
    expect_true(all(x$count == 2L))
    expect_equal(x$date_index, dates)

    # no groupings but with count
    x <- incidence(dat, date_index = "date", counts = "count")
    expect_s3_class(x, c("incidence2", "data.frame"), exact = TRUE)
    expect_equal(nrow(x), 731L)
    expect_equal(sum(x$count == 2L), 366L)
    expect_equal(sum(x$count == 4L), 365L)
    expect_equal(x$date_index, dates)

})

test_that("incidence with groupings but no intervals works", {
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

    # groupings and no counts
    x <- incidence(dat, date_index = "dates", groups = c("height", "size"))
    expect_equal(x$date_index, rep(dates, each = 2))
    x <- incidence(dat, date_index = "dates", groups = "size")
    expect_equal(sum(x$count == 2L), 28L)

    # groupings and counts
    x <- incidence(dat, date_index = "dates", groups = "size", counts = "count")
    expected_count <- c(rep(2L,14L), rep(4L, 14L))
    expect_equal(x$count, expected_count)
    expect_equal(x$size, size)
})

test_that("incidence with mutiple date indices but no intervals works", {
    firstday <- as.Date("2021-01-01")
    lastday <- as.Date("2021-12-31")
    dates_1 <- seq.Date(from = firstday, to = lastday, by = "day")
    dates_2 <- dates_1 - 31
    dates_1 <- as.POSIXlt(dates_1, tz="UTC")
    dates_1$mday <- 1
    dates_1 <- as.Date.POSIXlt(dates_1)
    dates_2 <- as.POSIXlt(dates_2, tz="UTC")
    dates_2$mday <- 1
    dates_2 <- as.Date.POSIXlt(dates_2)
    dat <- data.frame(dates_1, dates_2)
    x <- incidence(dat, date_index = c(deaths = "dates_1", onset = "dates_2"))
    x <- x[do.call(order, .subset(x, c("count_variable", "date_index"))), ]

    expected_dates <- seq(as.Date("2021-01-01"), as.Date("2021-12-01"), "month")
    expected_dates <- c(expected_dates, seq(as.Date("2020-12-01"), as.Date("2021-11-01"), "month"))
    expected_deaths <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
    expected_onsets <- c(31L, 31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L)
    expect_equal(as.Date(x$date_index), expected_dates)
    expect_equal(x$count,c(expected_deaths, expected_onsets))

    # incidence cannot work with different date_index types"
    dat <- data.frame(dates1 = c(1, 2), dates2 = Sys.Date() + 1:2)
    expect_error(incidence(dat, date_index = c(d1 = "dates1", d2 = "dates2")))

})

test_that("tibble, and data.frame input all match", {
    skip_if_not_installed("outbreaks")
    skip_if_not_installed("tibble")
    dat <- as.data.frame(outbreaks::covid19_england_nhscalls_2020)
    dat2 <- tibble::as_tibble(dat)
    i <- incidence(dat, "date", counts = "count", groups = "nhs_region")
    i2 <- incidence(dat2, "date", counts = "count", groups = "nhs_region")
    expect_identical(i,i2)
})

test_that("data.table, and data.frame input all match", {
    skip_if_not_installed("outbreaks")
    dat <- as.data.frame(outbreaks::covid19_england_nhscalls_2020)
    dat2 <- data.table::as.data.table(dat)
    i <- incidence(dat, "date", counts = "count", groups = "nhs_region")
    i2 <- incidence(dat2, "date", counts = "count", groups = "nhs_region")
    expect_identical(i,i2)
})

test_that("isoweek incidence with no groupings or count works", {
    firstday <- as.Date("2019-12-30") # Monday
    lastday <- as.Date("2021-12-29")  # Wednesday
    dates <- seq.Date(from = firstday, to = lastday, by = "day")
    dat <- data.frame(date = dates)
    dat_dates <- transform(dat, date = as_isoweek(date))
    x <- incidence(dat_dates, "date")
    expected_dates <- seq.Date(from = firstday, to = lastday, by = "7 days")
    expected_counts <- c(rep(7L, 104), 3L)
    expect_s3_class(x$date_index, "grates_isoweek")
    expect_equal(nrow(x), 105L)
    expect_equal(as.Date(x$date_index), expected_dates)
    expect_equal(x$count, expected_counts)

    expect_identical(
        incidence(dat, date_index = "date", interval = "yearweek"),
        x
    )

    expect_identical(
        incidence(dat, date_index = "date", interval = "isoweek"),
        x
    )
})

test_that("yearweek incidence with groups and without count works", {
    firstday <- as.Date("2021-02-01") # monday
    lastday <- as.Date("2021-02-28")  # sunday
    dates <- seq.Date(from = firstday, to = lastday, by = "day")
    count <- c(rep(1L, 14), rep(2L, 14))
    height <- c(rep("short", 14), rep("tall", 14))
    size <- c(rep("small", 7), rep("large", 21))
    dat <- data.frame(
        dates = as_yearweek(rep(dates, 2)),
        height = c(height, rev(height)),
        size = rep(size, 2),
        count = rep(count, 2)
    )
    x <- incidence(dat, date_index = "dates", groups = c("height", "size"))
    expected_dates <- seq.Date(from = firstday, to = lastday, by = "7 days")
    expected_dates <- rep(expected_dates, each = 2)
    expected_counts <- rep(7L, 8)
    expected_heights <- rep(c("short", "tall"), 4)
    expected_sizes <- c(rep("small", 2), rep("large", 6))
    expect_s3_class(x$date_index, "grates_yearweek_monday")
    expect_equal(nrow(x), 8L)
    expect_equal(as.Date(x$date_index), expected_dates)
    expect_equal(x$count, expected_counts)
    expect_equal(x$height, expected_heights)
    expect_equal(x$size, expected_sizes)
})

test_that("yearweek incidence with groups and count works", {
    firstday <- as.Date("2021-01-31") # sunday
    lastday <- as.Date("2021-02-27")  # saturday
    dates <- seq.Date(from = firstday, to = lastday, by = "day")
    count <- c(rep(1L, 14), rep(2L, 14))
    height <- c(rep("short", 14), rep("tall", 14))
    size <- c(rep("small", 7), rep("large", 21))
    dat <- data.frame(
        dates = as_yearweek(rep(dates, 2), firstday = 7),
        height = c(height, rev(height)),
        size = rep(size, 2),
        count = rep(count, 2)
    )
    x <- incidence(dat, date_index = "dates", groups = c("height", "size"), counts = "count")
    expected_dates <- seq.Date(from = firstday, to = lastday, by = "7 days")
    expected_dates <- rep(expected_dates, each = 2)
    expected_counts <- c(rep(7L, 4), rep(14L, 4))
    expected_heights <- rep(c("short", "tall"), 4)
    expected_sizes <- c(rep("small", 2), rep("large", 6))
    expect_s3_class(x$date_index, "grates_yearweek_sunday")
    expect_equal(nrow(x), 8L)
    expect_equal(as.Date(x$date_index), expected_dates)
    expect_equal(x$count, expected_counts)
    expect_equal(x$height, expected_heights)
    expect_equal(x$size, expected_sizes)
})

test_that("yearweek incidence with no groupings but with count works", {
    firstday <- as.Date("2019-12-30") # Monday
    lastday <- as.Date("2021-12-29")  # Wednesday
    dates <- seq.Date(from = firstday, to = lastday, by = "day")
    count <- c(rep(1L, 366), rep(2L, 365))
    dat_dates <- data.frame(date = as_isoweek(dates), count = count)
    x <- incidence(dat_dates, date_index = "date", counts = "count")
    expected_dates <- seq.Date(from = firstday, to = lastday, by = "7 days")
    expected_counts <- c(rep(7L, 52), 12L, rep(14L, 51), 6L)
    expect_s3_class(x$date_index, "grates_isoweek")
    expect_equal(nrow(x), 105L)
    expect_equal(as.Date(x$date_index), expected_dates)
    expect_equal(x$count, expected_counts)
})

test_that("yearmonth incidence with no groupings and without count works", {
    firstday <- as.Date("2020-01-01") # Wednesday
    lastday <- as.Date("2021-12-31")  # Friday
    dates <- seq.Date(from = firstday, to = lastday, by = "day")
    dat <- data.frame(date = as_yearmonth(dates))
    x <- incidence(dat, date_index = "date")
    expected_dates <- seq.Date(from = firstday, to = lastday, by = "1 month")
    expected_counts <- c(
        31L, 29L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L,
        31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L
    )
    expect_s3_class(x$date_index, "grates_yearmonth")
    expect_equal(nrow(x), 24L)
    expect_equal(as.Date(x$date_index), expected_dates)
    expect_equal(x$count, expected_counts)

    dat2 <- data.frame(date = dates)
    expect_identical(
        incidence(dat2, date_index = "date", interval = "month"),
        x
    )
    expect_identical(
        incidence(dat2, date_index = "date", interval = "months"),
        x
    )
    expect_identical(
        incidence(dat2, date_index = "date", interval = "monthly"),
        x
    )
    expect_identical(
        incidence(dat2, date_index = "date", interval = "yearmonth"),
        x
    )

})

test_that("yearmonth incidence with no groupings but with count", {
    firstday <- as.Date("2020-01-01") # Wednesday
    lastday <- as.Date("2021-12-31")  # Friday
    dates <- seq.Date(from = firstday, to = lastday, by = "day")
    count <- c(rep(1L, 366), rep(2L, 365))
    dat <- data.frame(date = dates, count = count)
    x <- incidence(dat, date_index = "date", counts = "count", interval = "monthly")
    expected_dates <- seq.Date(from = firstday, to = lastday, by = "1 month")
    expected_counts <- c(
        31L, 29L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L,
        62L, 56L, 62L, 60L, 62L, 60L, 62L, 62L, 60L, 62L, 60L, 62L
    )
    expect_s3_class(x$date_index, "grates_yearmonth")
    expect_equal(nrow(x), 24L)
    expect_equal(as.Date(x$date_index), expected_dates)
    expect_equal(x$count, expected_counts)

})

test_that("yearquarter incidence with no groupings and without count works", {
    firstday <- as.Date("2020-01-01") # Monday
    lastday <- as.Date("2021-12-31")  # Wednesday
    dates <- seq.Date(from = firstday, to = lastday, by = "day")
    dat_dates <- data.frame(date = as_yearquarter(dates))
    x <- incidence(dat_dates, date_index = "date")
    expected_dates <- seq.Date(from = firstday, to = lastday, by = "1 quarter")
    expected_counts <- c(
        91L, 91L, 92L, 92L,
        90L, 91L, 92L, 92L
    )
    expect_s3_class(x$date_index, "grates_yearquarter")
    expect_equal(nrow(x), 8L)
    expect_equal(as.Date(x$date_index), expected_dates)
    expect_equal(x$count, expected_counts)

    dat <- data.frame(date = dates)
    expect_identical(
        incidence(dat, date_index = "date", interval = "quarter"),
        x
    )
    expect_identical(
        incidence(dat, date_index = "date", interval = "quarters"),
        x
    )
    expect_identical(
        incidence(dat, date_index = "date", interval = "quarterly"),
        x
    )
    expect_identical(
        incidence(dat, date_index = "date", interval = "yearquarter"),
        x
    )
})

test_that("yearquarter incidence with no groupings but with count works", {
    firstday <- as.Date("2020-01-01") # Monday
    lastday <- as.Date("2021-12-31")  # Wednesday
    dates <- seq.Date(from = firstday, to = lastday, by = "day")
    count <- c(rep(1L, 366), rep(2L, 365))
    dat_dates <- data.frame(date = as_yearquarter(dates), count = count)
    x <- incidence(dat_dates, date_index = "date", counts = "count")

    expected_dates <- seq.Date(from = firstday, to = lastday, by = "1 quarter", counts = count)
    expected_counts <- c(
        91L, 91L, 92L, 92L,
        180L, 182L, 184L, 184L
    )
    expect_s3_class(x$date_index, "grates_yearquarter")
    expect_equal(nrow(x), 8L)
    expect_equal(as.Date(x$date_index), expected_dates)
    expect_equal(x$count, expected_counts)


    dat <- data.frame(date = dates, count = count)
    expect_identical(
        incidence(dat, date_index = "date", interval = "quarter", counts = "count"),
        x
    )
    expect_identical(
        incidence(dat, date_index = "date", interval = "quarters", counts = "count"),
        x
    )
    expect_identical(
        incidence(dat, date_index = "date", interval = "quarterly", counts = "count"),
        x
    )
    expect_identical(
        incidence(dat, date_index = "date", interval = "yearquarter", counts = "count"),
        x
    )
})

test_that("year incidence with no groupings and without count works", {
    firstday <- as.Date("2020-01-01") # Monday
    lastday <- as.Date("2021-12-31")  # Wednesday
    dates <- seq.Date(from = firstday, to = lastday, by = "day")
    dat_dates <- data.frame(date = as_year(dates))
    x <- incidence(dat_dates, date_index = "date")
    expected_dates <- seq.Date(from = firstday, to = lastday, by = "1 year")
    expected_counts <- c(366L, 365L)
    expect_s3_class(x$date_index, "grates_year")
    expect_equal(nrow(x), 2L)
    expect_equal(as.Date(x$date_index), expected_dates)
    expect_equal(x$count, expected_counts)

    dat <- data.frame(date = dates)
    expect_identical(
        incidence(dat, date_index = "date", interval = "year"),
        x
    )
    expect_identical(
        incidence(dat, date_index = "date", interval = "years"),
        x
    )
    expect_identical(
        incidence(dat, date_index = "date", interval = "yearly"),
        x
    )

})

test_that("year incidence with no groupings but with a count works", {
    firstday <- as.Date("2020-01-01") # Monday
    lastday <- as.Date("2021-12-31")  # Wednesday
    dates <- seq.Date(from = firstday, to = lastday, by = "day")
    count <- c(rep(1L, 366), rep(2L, 365))
    dat_dates <- data.frame(date = as_year(dates), count = count)
    x <- incidence(dat_dates, date_index = "date", counts = "count")
    expected_dates <- seq.Date(from = firstday, to = lastday, by = "1 year", counts = "count")
    expected_counts <- c(366, 730L)
    expect_s3_class(x$date_index, "grates_year")
    expect_equal(nrow(x), 2L)
    expect_equal(as.Date(x$date_index), expected_dates)
    expect_equal(x$count, expected_counts)


    dat <- data.frame(date = dates, count = count)
    expect_identical(
        incidence(dat, date_index = "date", interval = "year", counts = "count"),
        x
    )
    expect_identical(
        incidence(dat, date_index = "date", interval = "years", counts = "count"),
        x
    )
    expect_identical(
        incidence(dat, date_index = "date", interval = "yearly", counts = "count"),
        x
    )

})
