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
