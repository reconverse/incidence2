test_that("complete_counts works", {

  dat <- data.frame(
    dates = Sys.Date() + 1:4,
    groups1 = rep(c("bob","george"), 2),
    groups2 = c(rep("groupa", 2), rep("groupb", 2)),
    counts1 = 1:4,
    counts2 = 11:14
  )

  i <- incidence(
    dat,
    date_index = dates,
    groups = c(groups1, groups2),
    counts = c(counts1, counts2)
  )

  res <- complete_counts(i, fill = NA)
  expect_equal(nrow(res), 16)
  expect_equal(res[complete.cases(res),], i)
  res2 <- complete_counts(i)
  expect_equal(sum(res2$counts1), sum(i$counts1))
  expect_equal(sum(res2$counts2), sum(i$counts2))
  expect_equal(nrow(res2), 16)
})


