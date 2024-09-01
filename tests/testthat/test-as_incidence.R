test_that("Default dispatch gives error", {
    expect_snapshot(error = TRUE, as_incidence(mtcars))
})

test_that("Default dispatch gives good error message", {
    expect_snapshot(error = TRUE, as_incidence(mtcars))
})

test_that("as_incidence works as expected", {
    skip_if_not_installed("outbreaks")
    dat <- outbreaks::ebola_sim_clean$linelist
    i <- incidence(dat, date_index = "date_of_onset", groups = c("gender", "hospital"))
    expect_identical(as_incidence(i), i)
})
