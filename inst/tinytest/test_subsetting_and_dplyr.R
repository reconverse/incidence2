if (require(dplyr) && require(outbreaks)) {
    data(ebola_sim_clean, package = "outbreaks")
    dat <- ebola_sim_clean$linelist

    # create incidence object
    dat$date_of_onset <- as_period(dat$date_of_onset, n = 14L, offset = as.Date("2014-05-20"))
    inci <- incidence(
        dat,
        date_index = "date_of_onset",
        groups = c("hospital", "gender")
    )

    # dplyr operations preserve class
    x <- filter(inci, gender == "f", hospital == "Rokupa Hospital")
    expect_inherits(x, "incidence")

    x <- slice_sample(inci, prop = 0.1)
    expect_inherits(x, "incidence")

    x <- slice_sample(inci, n = 10)
    expect_inherits(x, "incidence")

    x <- slice(inci, 1, 5, 10)
    expect_inherits(x, "incidence")

    x <- slice_max(inci, order_by = count, n = 10)
    expect_inherits(x, "incidence")

    x <- select(inci, everything())
    expect_inherits(x, "incidence")

    x <- mutate(inci, future = date_index + 999)
    expect_inherits(x, "incidence")

    x <- rename(inci, left_bin = date_index)
    expect_inherits(x, "incidence")

    expect_inherits(inci[], "incidence")

    # Adding rows that are multiple of 2 weeks maintains class
    x <-
        inci %>%
        slice_head(n = 2) %>%
        mutate(date_index = date_index + 112) %>%
        bind_rows(inci)
    expect_true(inherits(x, "incidence"))

    # operations that drop class
    expect_false(inherits(inci[1:3], "incidence"))

    x <- select(inci, date_index, count)
    expect_false(inherits(x, "incidence"))

    x <- select(inci, -1)
    expect_false(inherits(x, "incidence"))

    x <- pull(inci, 1)
    expect_false(inherits(x, "incidence"))

    x <- transmute(inci, new_count = count + 1)
    expect_false(inherits(x, "incidence"))

    # Adding duplicate rows drops class
    x <-
        inci %>%
        slice_head(n = 1) %>%
        bind_rows(inci)
    expect_false(inherits(x, "incidence"))
}

