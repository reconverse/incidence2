set.seed(1)
int <- sample(-3L:50L, 100, replace = TRUE)
dates <- as.Date("2018-01-31") + int
group_1 <- sample(letters[1:3], length(dates), replace = TRUE)
group_2 <- sample(letters[1:3], length(dates), replace = TRUE)
dat <- data.frame(dates, group_1, group_2)
x  <- incidence(dat, date_index = "dates", interval = "week", groups = group_1)
x2  <- incidence(dat, date_index = "dates", interval = "2 weeks", groups = group_1)


test_that("get_group_names works", {
  expect_equal(get_group_names(x2), "group_1")
  expect_error(get_group_names("test"), "Not implemented for class character")
})

test_that("get_dates works", {
  expect_equal(get_dates(x2), x2$date_index)
  expect_error(get_dates("test"), "Not implemented for class character")
})

test_that("get_dates_name works", {
  expect_equal(get_dates_name(x2), c("date_index"))
  expect_error(get_dates_name("test"), "Not implemented for class character")
})


test_that("get_counts works", {
  expect_equal(get_counts(x2), x2$count)
  expect_error(get_counts("test"), "Not implemented for class character")
})

test_that("get_count_names works", {
  expect_equal(get_count_names(x2), "count")
  expect_error(get_count_names("test"), "Not implemented for class character")
})


test_that("get_n works", {
  expect_equal(get_n(x2), c(count=100L))
  expect_error(get_n("test"), "Not implemented for class character")
})


test_that("get_interval works", {
  expect_equal(get_interval(x2, integer = TRUE), 14L)
  expect_equal(get_interval(x2), "2 monday weeks")
  expect_error(get_interval("test"), "Not implemented for class character")

  month_dates <- as.Date("2019-01-1") + 0:30
  month_x  <- incidence(data.frame(month_dates),
                        date_index = "month_dates",
                        interval = "month")
  expect_equal(get_interval(month_x, integer = TRUE), 31L)

  quarter_dates <- seq(from = as.Date("2019-01-01"),
                       to = as.Date("2019-03-31"),
                       by = 1L)
  quarter_x  <- incidence(data.frame(quarter_dates),
                        date_index = "quarter_dates",
                        interval = "quarter")
  expect_equal(get_interval(quarter_x, integer = TRUE), 90L)

  year_dates <- seq(from = as.Date("2019-01-01"),
                       to = as.Date("2019-12-31"),
                       by = 1L)
  year_x  <- incidence(data.frame(year_dates),
                          date_index = "year_dates",
                          interval = "year")
  expect_equal(get_interval(year_x, integer = TRUE), 365L)

})




