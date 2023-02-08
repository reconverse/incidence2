test_that("operations preserve class as expected", {
    # outbreaks required for all tests
    skip_if_not_installed("outbreaks")

    data(ebola_sim_clean, package = "outbreaks")
    dat <- ebola_sim_clean$linelist

    # create incidence object
    dat$date_of_onset <- as_period(dat$date_of_onset, n = 14L, offset = as.Date("2014-05-20"))
    inci <- incidence(
        dat,
        date_index = "date_of_onset",
        groups = c("hospital", "gender")
    )

    # selecting all columns
    expect_s3_class(inci[], "incidence2")

    # dplyr required from here onwards
    skip_if_not_installed("dplyr")

    # dplyr operations preserve class
    x <- dplyr::filter(inci, gender == "f", hospital == "Rokupa Hospital")
    expect_s3_class(x, "incidence2")

    x <- dplyr::slice_sample(inci, prop = 0.1)
    expect_s3_class(x, "incidence2")

    x <- dplyr::slice_sample(inci, n = 10)
    expect_s3_class(x, "incidence2")

    x <- dplyr::slice(inci, 1, 5, 10)
    expect_s3_class(x, "incidence2")

    x <- dplyr::slice_max(inci, order_by = count, n = 10)
    expect_s3_class(x, "incidence2")

    x <- dplyr::select(inci, everything())
    expect_s3_class(x, "incidence2")

    x <- dplyr::mutate(inci, future = date_index + 999)
    expect_s3_class(x, "incidence2")

    x <- dplyr::rename(inci, left_bin = date_index)
    expect_s3_class(x, "incidence2")

    # Adding rows that are multiple of 2 weeks maintains class
    x <-
        inci %>%
        dplyr::slice_head(n = 2) %>%
        dplyr::mutate(date_index = date_index + 112) %>%
        dplyr::bind_rows(inci)
    expect_s3_class(x, "incidence2")

})

test_that("operations drop class as expected", {

    # ourebreaks required for all tests
    skip_if_not_installed("outbreaks")

    data(ebola_sim_clean, package = "outbreaks")
    dat <- ebola_sim_clean$linelist

    # create incidence object
    dat$date_of_onset <- as_period(dat$date_of_onset, n = 14L, offset = as.Date("2014-05-20"))
    inci <- incidence(
        dat,
        date_index = "date_of_onset",
        groups = c("hospital", "gender")
    )

    # operations that drop class
    expect_false(inherits(inci[1:3], "incidence2"))

    # dplyr required from here onwards
    skip_if_not_installed("dplyr")

    x <- dplyr::select(inci, date_index, count)
    expect_false(inherits(x, "incidence2"))

    x <- dplyr::select(inci, -1)
    expect_false(inherits(x, "incidence2"))

    x <- dplyr::pull(inci, 1)
    expect_false(inherits(x, "incidence2"))

    x <- dplyr::transmute(inci, new_count = count + 1)
    expect_false(inherits(x, "incidence2"))

    # Adding duplicate rows drops class
    x <-
        inci %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::bind_rows(inci)
    expect_false(inherits(x, "incidence2"))
})













