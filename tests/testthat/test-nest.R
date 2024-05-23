test_that("nest works", {
    skip_if_not_installed("outbreaks")
    data(ebola_sim_clean, package = "outbreaks")
    dat <-
        ebola_sim_clean$linelist |>
        subset(!is.na(hospital)) |>
        incidence_(date_of_onset, hospital, interval = "isoweek")

    expect_identical(
        nest(dat),
        nest(as_tibble(dat), .by = c(count_variable, hospital))
    )
})
