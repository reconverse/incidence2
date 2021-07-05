test_that("cumulate gives deprecation error", {
  lifecycle::expect_defunct(cumulate(mtcars))
})
