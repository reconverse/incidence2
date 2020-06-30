context("Non-exported functions")

test_that("arg_values works", {
  expect_equal(arg_values("bob"), "bob")
  expect_equal(arg_values(bob), "bob")
  expect_equal(arg_values(c(bob, george)), c("bob", "george"))
  expect_equal(arg_values(c("bob", "george")), c("bob", "george"))
  expect_equal(arg_values(NULL), NULL)

})


test_that("check_dates works", {
  msg <- "NA detected in the dates"
  expect_error(check_dates(c(1, 2, NA), TRUE), msg)

  msg <- paste0(
    "Flooring from non-integer date caused approximations:\n",
    "Mean relative difference: 0.1"
  )
  expect_warning(check_dates(1.1), msg)

  msg <- paste0(
    "Input could not be converted to date. Accepted formats are:\n",
    "Date, POSIXct, integer, numeric"
  )
  expect_error(check_dates(factor("2001-01-01")), msg)

  x <- list(
    1L,
    as.POSIXct("2001-01-01"),
    as.Date("2001-01-01") + 1:10,
    1.0,
    100:1
  )
  for (e in x) {
    expect_equal(e, check_dates(e))
  }
})


test_that("check_presence works", {
  cols <- c("col1", "col2", "col3")
  nms <- c("bob", "col3", "col1", "george")
  msg <- paste("variable", c("bob", "george"), "not present in dataframe",
    collapse = "\n"
  )
  expect_error(check_presence(nms, cols), msg)
  nms <- c("col1", "col2", "col3", "col1")
  out <- expect_invisible(check_presence(nms, cols))
  expect_equal(out, NULL)
})


test_that("check_interval", {
  skip_on_cran()

  expect_error(
    check_interval(),
    "Interval is missing or NULL"
  )
  expect_error(
    check_interval(NULL),
    "Interval is missing or NULL"
  )
  expect_error(
    check_interval(1:2),
    "Exactly one value should be provided as interval \\(2 provided\\)"
  )
  expect_error(
    check_interval(integer(0)),
    "Exactly one value should be provided as interval \\(0 provided\\)"
  )
  expect_error(
    check_interval(NA),
    "Interval is not finite"
  )
  expect_error(
    check_interval(-Inf),
    "Interval is not finite"
  )
  expect_error(
    check_interval(.1),
    "Interval must be at least 1 \\(input: 0.100; after rounding: 0\\)"
  )
  expect_equal(check_interval(1), 1)
  expect_equal(check_interval(2.4), 2)
  expect_equal(check_interval(2.7), 3)
})
