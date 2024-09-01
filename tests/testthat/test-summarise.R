test_that("summarise works", {
    skip_if_not_installed("outbreaks")

    data(ebola_sim_clean, package = "outbreaks")
    dat <-
        ebola_sim_clean$linelist |>
        subset(!is.na(hospital)) |>
        incidence_(date_of_onset, hospital, interval = "isoweek")

    expect_identical(
        summarise(dat, sum(count)),
        summarise(as_tibble(dat), sum(count), .by = c(count_variable, hospital))
    )

    expect_error(summarise(dat, .by = "hospital"))
    expect_snapshot(error = TRUE, summarise(dat, .by = "hospital"))


    expect_error(summarise(dat, .groups = "keep"))
    expect_snapshot(error = TRUE, summarise(dat, .groups = "keep"))

})
