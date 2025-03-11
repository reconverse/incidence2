test_that("as.data.frame coercion works", {
    dates <- Sys.Date() + 1:10
    expected <- data.frame(date_index = dates, count_variable = "dates", count = 1L)
    x <- incidence(data.frame(dates), date_index = "dates")
    expect_identical(as.data.frame(x), expected)

    skip_if_not_installed("outbreaks")
    dat <- outbreaks::ebola_sim_clean$linelist
    i <- incidence(dat, date_index = "date_of_onset", groups = c("gender", "hospital"))
    expect_identical(
        as.data.frame(i),
        {
            tmp <- .subset(i)
            att <- attributes(tmp)
            old <- att[["names"]]
            attributes(tmp) <- NULL
            names(tmp) <- old
            list2DF(tmp)
        }
    )
})
