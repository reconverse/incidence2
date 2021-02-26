test_that("keep_first and keep_last works as expected", {
  firstday <- as.Date("2021-01-01")
  lastday <- as.Date("2021-12-31")
  dates <- seq.Date(from = firstday, to = lastday, by = "day")
  dat <- data.frame(dates)
  x <- incidence(dat, date_index = dates, interval = "month")
  fx <- keep_first(x, 3)
  lx <- keep_last(x, 3)

  expect_equal(as.Date(fx$date_index), seq(firstday, as.Date("2021-03-01"), by = "month"))
  expect_equal(fx$count, c(31L, 28L, 31L))

  expect_equal(as.Date(lx$date_index), seq(as.Date("2021-10-01"), lastday, by = "month"))
  expect_equal(lx$count, c(31L, 30L, 31L))

  wk <- as_yrmon(dates)
  expect_equal(keep_first(wk, 1), wk[1:31])
  expect_equal(keep_last(wk, 1), rev(wk)[1:31])

  expect_error(keep_first("bob"))
  expect_error(keep_last("bob"))
})
