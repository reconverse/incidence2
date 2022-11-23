library(grates)
set.seed(1)
int <- sample(-3:50, 100L, replace = TRUE)
dates <- as.Date("2018-01-31") + int
group_1 <- sample(letters[1:3], length(dates), replace = TRUE)
group_2 <- sample(letters[1:3], length(dates), replace = TRUE)
dat <- data.frame(dates, group_1, group_2)
dat$dates <- as_epiweek(dat$dates)
x <- incidence(dat, date_index = "dates", groups = c("group_1", "group_2"))

# regroup to know groups
expected <- incidence(dat, date_index = "dates")
expect_equal(regroup(x), expected)

# regroup to one group
expected <- incidence(dat, date_index = "dates", groups = "group_1")
expect_equal(regroup(x, "group_1"), expected)

# no groups
expected <- incidence(dat, date_index = "dates")
expect_equal(regroup(expected), expected)

# regroup none-incidence object
expect_error(regroup("test"), "`x` must be an <incidence> object.")
