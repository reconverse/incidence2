test_that("mutate works", {
    skip_if_not_installed("outbreaks")

    data(ebola_sim_clean, package = "outbreaks")
    dat <-
        ebola_sim_clean$linelist |>
        subset(!is.na(hospital)) |>
        incidence_(date_of_onset, hospital, interval = "isoweek")

    expect_identical(
        as_tibble(
            mutate(
                dat,
                ave = data.table::frollmean(count, n = 3L, align = "right")
            )
        ),

        mutate(
            as_tibble(dat),
            ave = data.table::frollmean(count, n = 3L, align = "right"),
            .by = c(count_variable, hospital)
        )
    )

})
