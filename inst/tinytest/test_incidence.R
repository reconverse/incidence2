# no groupings ------------------------------------------------------------
firstday <- as.Date("2020-01-01") # Wednesday
lastday <- as.Date("2021-12-31")  # Friday
dates <- seq.Date(from = firstday, to = lastday, by = "day")
count <- c(rep(1L, 366), rep(2L, 365))
dat <- data.frame(date = c(dates,dates), count = c(count, count))

# no groupings and no counts
x <- incidence(dat, date_index = "date")
expect_equal(class(x), c("incidence2", "data.frame"))
expect_equal(nrow(x), 731L)
expect_true(all(x$count == 2L))
expect_equal(x$date_index, dates)

# no groupings but with count
x <- incidence(dat, date_index = "date", counts = "count")
expect_equal(class(x), c("incidence2", "data.frame"))
expect_equal(nrow(x), 731L)
expect_equal(sum(x$count == 2L), 366L)
expect_equal(sum(x$count == 4L), 365L)
expect_equal(x$date_index, dates)


# with groupings ----------------------------------------------------------
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


# multiple date index -----------------------------------------------------
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

expected_dates <- seq(as.Date("2020-12-01"), as.Date("2021-11-01"), "month")
expected_dates <- c(expected_dates, seq(as.Date("2021-01-01"), as.Date("2021-12-01"), "month"))
expected_deaths <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
expected_onsets <- c(31L, 31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L)
expect_equal(as.Date(x$date_index), expected_dates,2)
expect_equal(x$count,c(expected_deaths, expected_onsets))

# incidence cannot work with different date_index types"
dat <- data.frame(dates1 = c(1, 2), dates2 = Sys.Date() + 1:2)
expect_error(incidence(dat, date_index = c(d1 = "dates1", d2 = "dates2")))


# tibble, data.table and data.frame input all match -----------------------
dat <- as.data.frame(outbreaks::covid19_england_nhscalls_2020)
dat2 <- data.table::as.data.table(dat)
dat3 <- dplyr::as_tibble(dat)
i <- incidence(dat, "date", counts = "count", groups = "nhs_region")
i2 <- incidence(dat2, "date", counts = "count", groups = "nhs_region")
i3 <- incidence(dat3, "date", counts = "count", groups = "nhs_region")
expect_identical(i,i2)
expect_identical(i,i3)
