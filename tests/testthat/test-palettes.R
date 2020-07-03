context("Color palettes")

test_that("incidence_pal", {
  skip_on_cran()

  expect_error(incidence_pal(NULL), "n is not a number")
  for (n in 1:20) {
    expect_length(incidence_pal(n), n)
  }

  expect_length(unique(incidence_pal(20)), 20)
})

