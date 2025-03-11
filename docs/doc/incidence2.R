data.table::setDTthreads(2)
litedown::reactor(error = TRUE, message = TRUE, print = NA, fig.dim = c(8, 5))
set.seed(1)

library(incidence2)

# linelist from the simulated ebola outbreak  (removing some missing entries)
ebola <- subset(outbreaks::ebola_sim_clean$linelist ,!is.na(hospital))
str(ebola)

(daily_incidence <- incidence(ebola, date_index = "date_of_onset"))

plot(daily_incidence)

(weekly_incidence <- 
    ebola |>
    mutate(date_of_onset = as_isoweek(date_of_onset)) |> 
    incidence(date_index = "date_of_onset"))
plot(weekly_incidence, border_colour = "white")

(dat <- incidence(ebola, date_index = "date_of_onset", interval = "isoweek"))
# check equivalent
identical(dat, weekly_incidence)

(weekly_incidence_gender <- incidence(
    ebola,
    date_index = "date_of_onset",
    groups = "gender",
    interval = "isoweek"
))

plot(weekly_incidence_gender, border_colour = "white", angle = 45)

plot(weekly_incidence_gender, border_colour = "white", angle = 45, fill = "gender")

(weekly_multi_dates <- incidence(
    ebola,
    date_index = c(
        onset = "date_of_onset",
        infection = "date_of_infection"
    ), 
    interval = "isoweek",
    groups = "gender"
))

summary(weekly_multi_dates)

plot(weekly_multi_dates, angle = 45, border_colour = "white")

plot(weekly_multi_dates, angle = 45, border_colour = "white", fill = "count_variable")

covid <- subset(
    covidregionaldataUK,
    !region %in% c("England", "Scotland", "Northern Ireland", "Wales")
)
str(covid)

monthly_covid <- incidence(
    covid,
    date_index = "date",
    groups = "region",
    counts = "cases_new",
    interval = "yearmonth"
)
monthly_covid

(monthly_covid <-
     covid |>
     tidyr::replace_na(list(cases_new = 0)) |> 
     incidence(
         date_index = "date",
         groups = "region",
         counts = "cases_new",
         interval = "yearmonth"
     ))
plot(monthly_covid, nrow = 3, angle = 45, border_colour = "white")

dat <- ebola[160:180, ]

(small <- incidence(
    dat,
    date_index = "date_of_onset",
    date_names_to = "date"
))
plot(small, show_cases = TRUE, angle = 45, n_breaks = 10)

(small_gender <- incidence(
    dat,
    date_index = "date_of_onset",
    groups = "gender",
    date_names_to = "date"
)) 
plot(small_gender, show_cases = TRUE, angle = 45, n_breaks = 10, fill = "gender")

# generate an incidence object with 3 groups
(x <- incidence_(
    ebola,
    date_index = date_of_onset,
    groups = c(gender, hospital, outcome),
    interval = "isoweek"
))
# regroup to just two groups
regroup_(x, c(gender, outcome))
# standard (non-tidy-select) version
regroup(x, c("gender", "outcome"))
# drop all groups
regroup(x)

dat <- data.frame(
    dates = as.Date(c("2020-01-01", "2020-01-04")),
    gender = c("male", "female")
)

(incidence <- incidence_(dat, date_index = dates, groups = gender))
complete_dates(incidence)

weekly_incidence <- incidence_(
    ebola,
    date_index = date_of_onset,
    groups = hospital,
    interval = "isoweek"
)

keep_first(weekly_incidence, 3)
keep_last(weekly_incidence, 3)

keep_peaks(weekly_incidence)

influenza <- incidence_(
    outbreaks::fluH7N9_china_2013,
    date_index = date_of_onset,
    groups = province
)

# across provinces (we suspend progress bar for markdown)
estimate_peak(influenza, progress = FALSE) |> 
    select(-count_variable)
# regrouping for overall peak
plot(regroup(influenza))
estimate_peak(regroup(influenza), progress = FALSE) |> 
    select(-count_variable)
# return the first peak of the grouped and ungrouped data
first_peak(influenza)
first_peak(regroup(influenza))
# bootstrap a single sample
bootstrap_incidence(influenza)

(y <- cumulate(weekly_incidence))
plot(y, angle = 45, nrow = 3)

# create a weekly incidence object
weekly_incidence <- incidence_(
    ebola,
    date_index = date_of_onset,
    groups = c(gender, hospital),
    interval = "isoweek"
)

# filtering preserves class
weekly_incidence |> 
    subset(gender == "f" & hospital == "Rokupa Hospital") |> 
    class()

class(weekly_incidence[c(1L, 3L, 5L), ])

# Adding columns preserve class
weekly_incidence$future <- weekly_incidence$date_index + 999L
class(weekly_incidence)
weekly_incidence |> 
    mutate(past = date_index - 999L) |> 
    class()

# rename preserve class
names(weekly_incidence)[names(weekly_incidence) == "date_index"] <- "isoweek"
str(weekly_incidence)

# select returns a tibble unless all date, count and group variables are
# preserved in the output
str(weekly_incidence[,-1L])
str(weekly_incidence[, -6L])

# duplicating rows will drop the class but only if duplicate rows
class(rbind(weekly_incidence, weekly_incidence))
class(rbind(weekly_incidence[1:5, ], weekly_incidence[6:10, ]))

# the name of the date_index variable of x
get_date_index_name(weekly_incidence)
# alias for `get_date_index_name()`
get_dates_name(weekly_incidence)
# the name of the count variable of x
get_count_variable_name(weekly_incidence)
# the name of the count value of x
get_count_value_name(weekly_incidence)
# the name(s) of the group variable(s) of x
get_group_names(weekly_incidence)
# the date_index variable of x
str(get_date_index(weekly_incidence))
# alias for get_date_index
str(get_dates(weekly_incidence))
# the count variable of x
str(get_count_variable(weekly_incidence))
# the count value of x
str(get_count_value(weekly_incidence))
# list of the group variable(s) of x
str(get_groups(weekly_incidence))

# first twenty weeks of the ebola data set across hospitals
dat <- incidence_(ebola, date_of_onset, groups = hospital, interval = "isoweek")
dat <- keep_first(dat, 20L)

# fit a poisson model to the grouped data
(fitted <-
    dat |>
    nest(.key = "data") |>
    mutate(
        model  = lapply(
            data,
            function(x) glm(count ~ date_index, data = x, family = poisson)
        )
    ))
# Add confidence intervals to the result
(intervals <-
    fitted |>
    mutate(result = Map(
        function(data, model) {
            data |>
                ciTools::add_ci(
                    model,
                    alpha = 0.05,
                    names = c("lower_ci", "upper_ci")
                ) |>
                as_tibble()
        },
        data,
        model
    )) |>
    select(hospital, result) |>
    unnest(result))
# plot
plot(dat, angle = 45) +
    ggplot2::geom_line(
        ggplot2::aes(date_index, y = pred),
        data = intervals,
        inherit.aes = FALSE
    ) +
    ggplot2::geom_ribbon(
        ggplot2::aes(date_index, ymin = lower_ci, ymax = upper_ci),
        alpha = 0.2,
        data = intervals,
        inherit.aes = FALSE,
        fill = "#BBB67E"
    )

weekly_incidence |>
    regroup_(hospital) |> 
    mutate(rolling_average = data.table::frollmean(count, n = 3L, align = "right")) |> 
    plot(border_colour = "white", angle = 45) +
    ggplot2::geom_line(ggplot2::aes(x = date_index, y = rolling_average))

