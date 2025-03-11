test_that("as_tibble coercion works", {
    dates <- Sys.Date() + 1:10
    expected <- tibble::tibble(date_index = dates, count_variable = "dates", count = 1L)
    x <- incidence(data.frame(dates), date_index = "dates")
    expect_identical(as_tibble(x), expected)

    skip_if_not_installed("outbreaks")
    dat <- outbreaks::ebola_sim_clean$linelist
    i <- incidence(dat, date_index = "date_of_onset", groups = c("gender", "hospital"))
    expect_identical(as_tibble(i), as_tibble(as.data.frame(i)))
})
