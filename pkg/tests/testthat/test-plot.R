save_png <- function(code, width = 600, height = 400) {
    path <- tempfile(fileext = ".png")
    png(path, width = width, height = height)
    on.exit(dev.off())
    print(code)
    path
}

expect_snapshot_plot <- function(name, code) {
    skip_if_not_installed("ggplot2", "2.0.0")
    skip_if_not_installed("outbreaks")
    skip_on_os("windows", "mac")
    skip_on_ci()
    name <- paste0(name, ".png")

    # Announce the file before touching `code`. This way, if `code`
    # unexpectedly fails or skips, testthat will not auto-delete the
    # corresponding snapshot file.
    announce_snapshot_file(name = name)

    path <- save_png(code)
    expect_snapshot_file(path, name)
}

test_that("plotting works", {
    ebola <- subset(outbreaks::ebola_sim_clean$linelist ,!is.na(hospital))
    daily_incidence <- incidence(ebola, date_index = "date_of_onset")
    daily_incidence_plot <- plot(daily_incidence)
    expect_snapshot_plot("daily_incidence_plot", daily_incidence_plot)

    weekly_incidence <-
        ebola |>
        mutate(date_of_onset = as_isoweek(date_of_onset)) |>
        incidence(date_index = "date_of_onset")
    weekly_incidence_plot <- plot(weekly_incidence, border_colour = "white")
    expect_snapshot_plot("weekly_incidence_plot", weekly_incidence_plot)


    weekly_incidence_gender <- incidence(
        ebola,
        date_index = "date_of_onset",
        groups = "gender",
        interval = "isoweek"
    )
    weekly_incidence_gender_plot <- plot(weekly_incidence_gender, border_colour = "white", angle = 45)
    expect_snapshot_plot("weekly_incidence_gender_plot", weekly_incidence_gender_plot)
    weekly_incidence_gender_plot_filled <- plot(weekly_incidence_gender, border_colour = "white", angle = 45, fill = "gender")
    expect_snapshot_plot("weekly_incidence_gender_plot_filled", weekly_incidence_gender_plot_filled)

    weekly_multi_dates <- incidence(
        ebola,
        date_index = c(
            onset = "date_of_onset",
            infection = "date_of_infection"
        ),
        interval = "isoweek",
        groups = "gender"
    )

    weekly_multi_dates_plot <- plot(weekly_multi_dates, angle = 45, border_colour = "white")
    expect_snapshot_plot("weekly_multi_dates_plot", weekly_multi_dates_plot)
    weekly_multi_dates_plot_filled <- plot(weekly_multi_dates, angle = 45, border_colour = "white", fill = "count_variable")
    expect_snapshot_plot("weekly_multi_dates_plot_filled", weekly_multi_dates_plot_filled)

    covid <- subset(
        covidregionaldataUK,
        !region %in% c("England", "Scotland", "Northern Ireland", "Wales")
    )
    monthly_covid <-
        covid |>
        tidyr::replace_na(list(cases_new = 0)) |>
        incidence(
            date_index = "date",
            groups = "region",
            counts = "cases_new",
            interval = "yearmonth"
        )
    monthly_covid_plot <- plot(monthly_covid, nrow = 3, angle = 45, border_colour = "white")
    expect_snapshot_plot("monthly_covid_plot", monthly_covid_plot)

    dat <- ebola[160:180, ]

    small <- incidence(
        dat,
        date_index = "date_of_onset",
        date_names_to = "date"
    )
    epiet_plot <- plot(small, show_cases = TRUE, angle = 45, n_breaks = 10)
    expect_snapshot_plot("epiet_plot", epiet_plot)

    small_gender <- incidence(
        dat,
        date_index = "date_of_onset",
        groups = "gender",
        date_names_to = "date"
    )
    epiet_gender_plot <- plot(small_gender, show_cases = TRUE, angle = 45, n_breaks = 10, fill = "gender")
    expect_snapshot_plot("epiet_gender_plot", epiet_gender_plot)

    influenza <- incidence_(
        outbreaks::fluH7N9_china_2013,
        date_index = date_of_onset
    )
    influenza_plot <- plot(regroup(influenza))
    expect_snapshot_plot("influenza_plot", influenza_plot)

    y <- cumulate(weekly_incidence)
    cumulate_plot <- plot(y, angle = 45, nrow = 3)
    expect_snapshot_plot("cumulate_plot", cumulate_plot)

    rolling_average_plot <-
        weekly_incidence |>
        mutate(rolling_average = data.table::frollmean(count, n = 3L, align = "right")) |>
        plot(border_colour = "white", angle = 45) +
        ggplot2::geom_line(ggplot2::aes(x = date_index, y = rolling_average))
    suppressWarnings(expect_snapshot_plot("rolling_average_plot", rolling_average_plot))

})
