context("Color palettes")

test_that("vibrant", {
  skip_on_cran()

  expect_error(vibrant(NULL), "n is not a number")
  for (n in c(1, 5, 30)) {
    expect_length(vibrant(n), n)
  }

  expect_length(unique(vibrant(20)), 20)
})



test_that("muted", {
  skip_on_cran()

  expect_error(muted(NULL), "n is not a number")
  for (n in c(1, 5, 30)) {
    expect_length(muted(n), n)
  }

  expect_length(unique(muted(20)), 20)
})

