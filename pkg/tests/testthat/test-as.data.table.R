test_that("as.data.table coercion works", {
    dates <- Sys.Date() + 1:10
    expected <- data.table::data.table(date_index = dates, count_variable = "dates", count = 1L)
    x <- incidence(data.frame(dates), date_index = "dates")
    expect_true(all.equal(as.data.table(x), expected))

    skip_if_not_installed("outbreaks")
    dat <- outbreaks::ebola_sim_clean$linelist
    i <- incidence(dat, date_index = "date_of_onset", groups = c("gender", "hospital"))
    expect_true(all.equal(as.data.table(i), as.data.table(as.data.frame(i))))
})
