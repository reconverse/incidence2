test_that("cumulate works", {

  int <- sample(-3L:50L, 100, replace = TRUE)
  dates <- as.Date("2018-01-31") + int
  group_1 <- sample(letters[1:3], length(dates), replace = TRUE)
  group_2 <- sample(letters[1:3], length(dates), replace = TRUE)
  dat <- data.frame(dates, group_1, group_2)
  x <- incidence(dat, date_index = "dates", interval = "2 weeks", groups = group_1)
  xc <- x
  for (gr in group_1) {
    idx <- x$group_1 == gr
    xc[idx, "count"] <- cumsum(x[idx, "count"])
  }
  attr(xc, "cumulative") <- TRUE
  xc <- xc[order(xc$group_1, xc$date_index),]

  expect_equal(cumulate(x), xc)
  expect_equal(cumulate(1:3), c(1, 3, 6))
  expect_error(cumulate(cumulate(x)), "x is already a cumulative incidence")
})
