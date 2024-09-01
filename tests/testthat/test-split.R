test_that("split works", {
    skip_if_not_installed("outbreaks")

    data(ebola_sim_clean, package = "outbreaks")

    dat <-
        ebola_sim_clean$linelist |>
        subset(!is.na(hospital)) |>
        incidence_(date_of_onset, hospital, interval = "isoweek")

    out <- split(dat)

    expected <- vctrs::vec_split(as_tibble(dat), as_tibble(dat)[c("count_variable", "hospital")])

    expect_identical(c(out), expected$val)
    expect_identical(attr(out, "key"), expected$key)

    expect_error(split(dat, f = dat$hospital))
    expect_snapshot(error = TRUE, split(dat, f = dat$hospital))

    expect_error(split(dat, drop = TRUE))
    expect_snapshot(error = TRUE, split(dat, drop = TRUE))

})
