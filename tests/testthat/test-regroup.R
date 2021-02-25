test_that("regroup works", {
  set.seed(1)
  int <- sample(-3L:50L, 100, replace = TRUE)
  dates <- as.Date("2018-01-31") + int
  group_1 <- sample(letters[1:3], length(dates), replace = TRUE)
  group_2 <- sample(letters[1:3], length(dates), replace = TRUE)
  dat <- data.frame(dates, group_1, group_2)
  x <- incidence(dat, date_index = "dates",
                 interval = "2 weeks", groups = c(group_1, group_2))


  # regroup to know groups
  expected <- incidence(dat, date_index = "dates", interval = "2 weeks")
  expect_equal(regroup(x), expected)

  # regroup to one group
  expected <- incidence(dat, date_index = "dates",
                        interval = "2 weeks", groups = group_1)
  expect_equal(regroup(x, group_1), expected)

  # regroup none-incidence object
  expect_error(regroup("test"), "x should be an 'incidence2' object.")
})


