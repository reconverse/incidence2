context("Accessor functions")


int <- sample(-3L:50L, 100, replace = TRUE)
dates <- as.Date("2018-01-31") + int
group_1 <- sample(letters[1:3], length(dates), replace = TRUE)
group_2 <- sample(letters[1:3], length(dates), replace = TRUE)
dat <- data.frame(dates, group_1, group_2)
x  <- incidence(dat, date_index = "dates", interval = "2 weeks", groups = group_1)


test_that("get_group_vars works", {
  expect_equal(get_group_vars(x), "group_1")
  expect_error(get_group_vars("test"), "Not implemented for class character")
})


test_that("get_date_vars works", {
  expect_equal(get_date_vars(x), c("bin_date"))
  expect_error(get_date_vars("test"), "Not implemented for class character")
})

test_that("get_count_vars works", {
  expect_equal(get_count_vars(x), "count")
  expect_error(get_count_vars("test"), "Not implemented for class character")
})

test_that("get_interval works", {
  expect_equal(get_interval(x, integer = TRUE), 14L)
  expect_equal(get_interval(x), "2 weeks")
  expect_error(get_interval("test"), "Not implemented for class character")
})

test_that("get_timespan works", {
  span <- as.integer(max(x$bin_date) - min(x$bin_date) + 1)
  expect_equal(get_timespan(x), span)
  expect_error(get_timespan("test"), "Not implemented for class character")
})

test_that("get_n works", {
  expect_equal(get_n(x), 100L)
  expect_error(get_n("test"), "Not implemented for class character")
})




