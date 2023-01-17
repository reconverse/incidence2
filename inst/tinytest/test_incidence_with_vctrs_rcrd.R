if (require("clock", quietly = TRUE)) {
    # isoweek, no groupings and without count
    firstday <- as.Date("2019-12-30") # Monday
    lastday <- as.Date("2021-12-29")  # Wednesday
    dates <- seq(from = firstday, to = lastday, by = "day")
    count <- c(rep(1L, 366), rep(2L, 365))
    dat_dates <- data.frame(date = dates, count = count)
    dat_dates <- transform(
        dat_dates,
        date = calendar_narrow(
            as_iso_year_week_day(date),
            "week"
        )
    )
    x <- incidence(dat_dates, "date")
    expected_dates <- seq.Date(from = firstday, to = lastday, by = "7 days")
    expected_counts <- c(rep(7L, 104), 3L)
    expect_true(is_iso_year_week_day(x$date_index))
    expect_equal(nrow(x), 105L)
    expect_equal(as.Date(calendar_widen(x$date_index, "day")), expected_dates)
    expect_equal(x$count, expected_counts)

    # week, with groups and without count work
    firstday <- as.Date("2021-02-01") # monday
    lastday <- as.Date("2021-02-28")  # sunday
    dates <- seq.Date(from = firstday, to = lastday, by = "day")
    count <- c(rep(1L, 14), rep(2L, 14))
    height <- c(rep("short", 14), rep("tall", 14))
    size <- c(rep("small", 7), rep("large", 21))
    dat <- data.frame(
        dates = calendar_narrow(
            as_iso_year_week_day(rep(dates, 2)),
            "week"
        ),
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
    expect_true(is_iso_year_week_day(x$date_index))
    expect_equal(nrow(x), 8L)
    expect_equal(as.Date(calendar_widen(x$date_index, "day")), expected_dates)
    expect_equal(x$count, expected_counts)
    expect_equal(x$height, expected_heights)
    expect_equal(x$size, expected_sizes)

    # week, with groups and with count work
    firstday <- as.Date("2021-02-01") # monday
    lastday <- as.Date("2021-02-28")  # sunday
    dates <- seq.Date(from = firstday, to = lastday, by = "day")
    count <- c(rep(1L, 14), rep(2L, 14))
    height <- c(rep("short", 14), rep("tall", 14))
    size <- c(rep("small", 7), rep("large", 21))
    dat <- data.frame(
        dates = calendar_narrow(
            as_iso_year_week_day(rep(dates, 2)),
            "week"
        ),
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
    expect_true(is_iso_year_week_day(x$date_index))
    expect_equal(nrow(x), 8L)
    expect_equal(as.Date(calendar_widen(x$date_index,"day")), expected_dates)
    expect_equal(x$count, expected_counts)
    expect_equal(x$height, expected_heights)
    expect_equal(x$size, expected_sizes)

    # week, no groupings and with count
    firstday <- as.Date("2019-12-30") # Monday
    lastday <- as.Date("2021-12-29")  # Wednesday
    dates <- seq.Date(from = firstday, to = lastday, by = "day")
    count <- c(rep(1L, 366), rep(2L, 365))
    dat_dates <- data.frame(
        date = calendar_narrow(
            as_iso_year_week_day(dates),
            "week"
        ),
        count = count
    )
    x <- incidence(dat_dates, date_index = "date", counts = "count")
    expected_dates <- seq.Date(from = firstday, to = lastday, by = "7 days")
    expected_counts <- c(rep(7L, 52), 12L, rep(14L, 51), 6L)
    expect_true(is_iso_year_week_day(x$date_index))
    expect_equal(nrow(x), 105L)
    expect_equal(as.Date(calendar_widen(x$date_index,"day")), expected_dates)
    expect_equal(x$count, expected_counts)
}

