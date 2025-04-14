test_that("Fails for bad input", {
    firstday <- as.Date("2021-01-01")
    lastday <- as.Date("2021-12-31")
    dates <- seq.Date(from = firstday, to = lastday, by = "day")
    dat <- data.frame(dates = as_yearmonth(dates))
    x <- incidence(dat, date_index = "dates", count_names_to = "tmp___index")

    expect_error(keep_first("bob"))
    expect_error(keep_last("bob"))

    expect_snapshot(error = TRUE, keep_first("bob"))
    expect_snapshot(error = TRUE, keep_last("bob"))
})

test_that("keep_first works", {
    firstday <- as.Date("2021-01-01")
    lastday <- as.Date("2021-12-31")
    dates <- seq.Date(from = firstday, to = lastday, by = "day")
    dat <- data.frame(dates = as_yearmonth(dates))
    x <- incidence(dat, date_index = "dates")
    x <- x[sample.int(nrow(x)),]
    fx <- keep_first(x, 3)

    expect_identical(sort(as.Date(fx$date_index)), seq(firstday, as.Date("2021-03-01"), by = "month"))
    expect_identical(fx$count[order(fx$date_index)], c(31L, 28L, 31L))
})


test_that("keep_last works", {
    firstday <- as.Date("2021-01-01")
    lastday <- as.Date("2021-12-31")
    dates <- seq.Date(from = firstday, to = lastday, by = "day")
    dat <- data.frame(dates = as_yearmonth(dates))
    x <- incidence(dat, date_index = "dates")
    x <- x[sample.int(nrow(x)),]
    lx <- keep_last(x, 3)

    expect_equal(sort(as.Date(lx$date_index)), seq(as.Date("2021-10-01"), lastday, by = "month"))
    expect_equal(lx$count[order(lx$date_index)], c(31L, 30L, 31L))
})


test_that("Regresison test for hack removal ", {
    firstday <- as.Date("2021-01-01")
    lastday <- as.Date("2021-12-31")
    dates <- seq.Date(from = firstday, to = lastday, by = "day")
    dat <- data.frame(dates = as_yearmonth(dates))

    x <- incidence(dat, date_index = "dates", count_names_to = "tmp___index")
    expect_no_condition(keep_first(x, 3))
    expect_no_condition(keep_last(x, 3))
})

