context("Bootstrapping incidence")

set.seed(1)

dates <- as.integer(sample(-3:100, 50, replace = TRUE))
DATES <- as.Date("2016-09-20") + dates
groups <- sample(c("toto", "tata"), 50, replace = TRUE)
dat <- data.frame(DATES, groups = groups, stringsAsFactors = FALSE)


test_that("Bootstrap needs an incidence object", {
  expect_error(bootstrap(DATES), "x is not an incidence object")
})


test_that("estimate_peak needs an incidence object", {
  expect_error(estimate_peak(DATES), "x is not an incidence object")
})


test_that("Bootstrap incidence with groups", {
  skip_on_cran()

  x <- incidence(dat, date_index = DATES, interval = 3, groups = groups)
  y <- bootstrap(x)
  z <- bootstrap(x, randomise_groups = TRUE)

  expect_identical(sum(x$count), sum(y$count))

  expect_identical(sum(x$count), sum(z$count))

  expect_identical(names(x), names(y))

  expect_identical(names(x), names(z))

  expect_identical(attr(x, "interval"), attr(y, "interval"))

  expect_identical(attr(x, "interval"), attr(z, "interval"))

  expect_true(setequal(y$groups, z$groups))
})




context("Mountain Climbing")

test_that("find_peak can find the peak", {
  skip_on_cran()

  x <- incidence(dat, date_index = DATES, interval = 3, groups = groups)

  expect_error(find_peak(1:10), "`1:10` is not an incidence object")

  expect_message(p1 <- find_peak(x), "`x` is stratified by groups\nregrouping groups before finding peaks")

  expect_equal(nrow(p1), 1L)

  expect_equal(find_peak(x, regroup = FALSE)$groups, c("tata", "toto"))
})


test_that("estimate_peak can roughly estimate it", {

  x <- incidence(dat, date_index = DATES, interval = 3, groups = groups)

  dat2 <- data.frame(dates, stringsAsFactors = FALSE)
  y <- incidence(dat2, date_index = dates, interval = 3)


  expect_message(e1 <- estimate_peak(x), "x is stratified by groups\nregrouping groups before finding peaks")

  e2 <- estimate_peak(y)
  expect_named(e1, c("observed", "estimated", "ci", "peaks"))
  expect_named(e2, c("observed", "estimated", "ci", "peaks"))

  # The observed is identical to find_peak
  expect_identical(e1$observed, find_peak(regroup(x)))
  expect_identical(e2$observed, find_peak(regroup(y)))

  # The number of peaks defaults to 100
  expect_identical(nrow(e1$peaks), 100L)
  expect_identical(nrow(e2$peaks), 100L)

  # The observed falls within the confidence interval
  expect_gte(as.integer(e1$observed$date_group), as.integer(e1$ci[1]))
  expect_lte(as.integer(e1$observed$date_group), as.integer(e1$ci[2]))
  expect_gte(as.integer(e2$observed$date_group), as.integer(e2$ci[1]))
  expect_lte(as.integer(e2$observed$date_group), as.integer(e2$ci[2]))
})
