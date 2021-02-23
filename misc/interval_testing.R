library(outbreaks)
library(incidence2)
library(data.table)

dat <- ebola_sim_clean$linelist

# -------------------------------------------------------------------------
i <- incidence(dat, date_index = date_of_onset)
i
summary(i)
plot(i)


# -------------------------------------------------------------------------
i_weekly <- incidence(dat, date_index = date_of_onset, interval = "1 week")
i_weekly
summary(i_weekly)
plot(i_weekly)


# -------------------------------------------------------------------------
i_weekly_w <- incidence(dat, date_index = date_of_onset, interval = "1 wednesday week")
i_weekly_w
summary(i_weekly_w)
plot(i_weekly_w)


# -------------------------------------------------------------------------
i_biweekly <- incidence(dat, date_index = date_of_onset, interval = "2 saturday weeks")
i_biweekly
summary(i_biweekly)
plot(i_biweekly)


# -------------------------------------------------------------------------
i_monthly <- incidence(dat, date_index = date_of_onset, interval = "1 month")
i_monthly
summary(i_monthly)
plot(i_monthly)


# -------------------------------------------------------------------------
i_weekly_sex <- incidence(dat, date_index = date_of_onset,
                          interval = "1 weeks", groups = gender)
i_weekly_sex
summary(i_weekly_sex)
plot(i_weekly_sex)
facet_plot(i_weekly_sex, fill = gender, n_breaks = 6)



# -------------------------------------------------------------------------
inci <- incidence(dat, date_index = date_of_onset,
                  interval = "1 week", groups = c(outcome, hospital, gender))
inci %>%
  facet_plot(facets = hospital, fill = outcome, n_breaks = 8, nrow = 3)



# -------------------------------------------------------------------------
i_int <-
  dat %>%
  mutate(date_of_onset = as.integer(date_of_onset)) %>%
  incidence(date_index = date_of_onset, interval = 7)
i_int
summary(i_int)
plot(i_int)


# -------------------------------------------------------------------------
res <- incidence(
  dat,
  date_index = c(
    count_infection_date = date_of_infection,
    count_onset_date = date_of_onset
  ),
  groups = c(gender, hospital),
  interval = "1 week"
)

res
summary(res)
plot(res)
