library(grates)

# isoweek, no groupings and without count
firstday <- as.Date("2019-12-30") # Monday
lastday <- as.Date("2021-12-29")  # Wednesday
dates <- seq.Date(from = firstday, to = lastday, by = "day")
count <- c(rep(1L, 366), rep(2L, 365))
dat_dates <- data.frame(date = dates, count = count)
dat_dates <- transform(dat_dates, date = as_yearweek(date))
x <- incidence(dat_dates, "date")
expected_dates <- seq.Date(from = firstday, to = lastday, by = "7 days")
expected_counts <- c(rep(7L, 104), 3L)
expect_inherits(x$date_index, "grates_yearweek_monday")
expect_equal(nrow(x), 105L)
expect_equal(as.Date(x$date_index), expected_dates)
expect_equal(x$count, expected_counts)

# week, with groups and without count work
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
expect_inherits(x$date_index, "grates_yearweek_monday")
expect_equal(nrow(x), 8L)
expect_equal(as.Date(x$date_index), expected_dates)
expect_equal(x$count, expected_counts)
expect_equal(x$height, expected_heights)
expect_equal(x$size, expected_sizes)

# week, with groups and with count work
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
expect_inherits(x$date_index, "grates_yearweek_sunday")
expect_equal(nrow(x), 8L)
expect_equal(as.Date(x$date_index), expected_dates)
expect_equal(x$count, expected_counts)
expect_equal(x$height, expected_heights)
expect_equal(x$size, expected_sizes)

# TODO - got to here

# week, no groupings and with count
firstday <- as.Date("2019-12-30") # Monday
lastday <- as.Date("2021-12-29")  # Wednesday
dates <- seq.Date(from = firstday, to = lastday, by = "day")
count <- c(rep(1L, 366), rep(2L, 365))
dat_dates <- data.frame(date = as_isoweek(dates), count = count)
x <- incidence(dat_dates, date_index = "date", counts = "count")
expected_dates <- seq.Date(from = firstday, to = lastday, by = "7 days")
expected_counts <- c(rep(7L, 52), 12L, rep(14L, 51), 6L)
expect_inherits(x$date_index, "grates_isoweek")
expect_equal(nrow(x), 105L)
expect_equal(as.Date(x$date_index), expected_dates)
expect_equal(x$count, expected_counts)

# single month, no groupings and without count
firstday <- as.Date("2020-01-01") # Wednesday
lastday <- as.Date("2021-12-31")  # Friday
dates <- seq.Date(from = firstday, to = lastday, by = "day")
count <- c(rep(1L, 366), rep(2L, 365))
dat <- data.frame(date = as_yearmonth(dates), count = count)
x <- incidence(dat, date_index = "date")
expected_dates <- seq.Date(from = firstday, to = lastday, by = "1 month")
expected_counts <- c(
    31L, 29L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L,
    31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L
)

# class
expect_inherits(x$date_index, "grates_yearmonth")
expect_equal(nrow(x), 24L)
expect_equal(as.Date(x$date_index), expected_dates)
expect_equal(x$count, expected_counts)

# single month, no groupings and with count
x <- incidence(dat, date_index = "date", counts = "count")
expected_dates <- seq.Date(from = firstday, to = lastday, by = "1 month")
expected_counts <- c(
    31L, 29L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L,
    62L, 56L, 62L, 60L, 62L, 60L, 62L, 62L, 60L, 62L, 60L, 62L
)

# class
expect_inherits(x$date_index, "grates_yearmonth")

# results
expect_equal(nrow(x), 24L)
expect_equal(as.Date(x$date_index), expected_dates)
expect_equal(x$count, expected_counts)


# single quarter, no groupings and without count
firstday <- as.Date("2020-01-01") # Monday
lastday <- as.Date("2021-12-31")  # Wednesday
dates <- seq.Date(from = firstday, to = lastday, by = "day")
count <- c(rep(1L, 366), rep(2L, 365))
dat_dates <- data.frame(date = as_yearquarter(dates), count = count)
x <- incidence(dat_dates, date_index = "date")
expected_dates <- seq.Date(from = firstday, to = lastday, by = "1 quarter")
expected_counts <- c(
    91L, 91L, 92L, 92L,
    90L, 91L, 92L, 92L
)

# class
expect_inherits(x$date_index, "grates_yearquarter")

# results
expect_equal(nrow(x), 8L)
expect_equal(as.Date(x$date_index), expected_dates)
expect_equal(x$count, expected_counts)

# single quarter, no groupings and with count
x <- incidence(dat_dates, date_index = "date", counts = "count")
expected_dates <- seq.Date(from = firstday, to = lastday, by = "1 quarter", counts = count)
expected_counts <- c(
    91L, 91L, 92L, 92L,
    180L, 182L, 184L, 184L
)

# class
expect_inherits(x$date_index, "grates_yearquarter")

# results
expect_equal(nrow(x), 8L)
expect_equal(as.Date(x$date_index), expected_dates)
expect_equal(x$count, expected_counts)

# single year, no groupings and without count
dat_dates <- data.frame(date = as_year(dates), count = count)
x <- incidence(dat_dates, date_index = "date")
expected_dates <- seq.Date(from = firstday, to = lastday, by = "1 year")
expected_counts <- c(366L, 365L)

# class
expect_inherits(x$date_index, "grates_year")

# results
expect_equal(nrow(x), 2L)
expect_equal(as.Date(x$date_index), expected_dates)
expect_equal(x$count, expected_counts)


# single year, no groupings and with count
x <- incidence(dat_dates, date_index = "date", counts = "count")
expected_dates <- seq.Date(from = firstday, to = lastday, by = "1 year", counts = "count")
expected_counts <- c(366, 730L)

# class
expect_inherits(x$date_index, "grates_year")

# results
expect_equal(nrow(x), 2L)
expect_equal(as.Date(x$date_index), expected_dates)
expect_equal(x$count, expected_counts)
