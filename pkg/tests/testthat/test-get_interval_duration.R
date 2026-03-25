test_that("get_interval_duration works", {
    firstday <- as.Date("2020-01-01") # Monday
    lastday <- as.Date("2021-12-31")  # Wednesday
    dates <- seq.Date(from = firstday, to = lastday, by = "day")
    dat <- data.frame(dates = dates)
    i <- incidence(dat, date_index = "dates")
    ii <- incidence(dat, date_index = "dates", interval = "epiweek")
    expect_identical(get_interval_duration(i), rep(1, nrow(i)))
    expect_identical(get_interval_duration(ii), rep(7, nrow(ii)))
})
