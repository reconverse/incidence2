test_that("Fails bad input", {
    skip_if_not_installed("withr")
    withr::with_seed(seed = 1, {
        dates <- as.integer(sample(-3:100, 50, replace = TRUE))
        groups <- sample(c("toto", "tata"), 50, replace = TRUE)
    })
    DATES <- as.Date("2016-09-20") + dates
    dat <- data.frame(DATES, groups = groups, stringsAsFactors = FALSE)
    i <- incidence(dat, date_index = "DATES", interval = 3, groups = "groups")

    expect_error(estimate_peak(DATES))
    expect_error(estimate_peak(i, n = 1.5))
    expect_error(estimate_peak(i, alpha = NA_real_))
    expect_error(estimate_peak(i, alpha = "bob"))
    expect_error(estimate_peak(i, first_only = "bob"))
    expect_error(estimate_peak(i, progress = "bob"))
})

test_that("Fails with good error for bad input", {
    skip_if_not_installed("withr")
    withr::with_seed(seed = 1, {
        dates <- as.integer(sample(-3:100, 50, replace = TRUE))
        groups <- sample(c("toto", "tata"), 50, replace = TRUE)
    })
    DATES <- as.Date("2016-09-20") + dates
    dat <- data.frame(DATES, groups = groups, stringsAsFactors = FALSE)
    i <- incidence(dat, date_index = "DATES", interval = 3, groups = "groups")

    expect_snapshot(error = TRUE, estimate_peak(DATES))
    expect_snapshot(error = TRUE, estimate_peak(i, n = 1.5))
    expect_snapshot(error = TRUE, estimate_peak(i, alpha = NA_real_))
    expect_snapshot(error = TRUE, estimate_peak(i, alpha = "bob"))
    expect_snapshot(error = TRUE, estimate_peak(i, first_only = "bob"))
    expect_snapshot(error = TRUE, estimate_peak(i, progress = "bob"))
})

test_that("estimate_peak can roughly estimate it", {

    skip_if_not_installed("withr")
    withr::with_seed(seed = 1, {
        dates <- as.integer(sample(-3:100, 50, replace = TRUE))
        groups <- sample(c("toto", "tata"), 50, replace = TRUE)
    })
    DATES <- as.Date("2016-09-20") + dates
    dat <- data.frame(DATES, groups = groups, stringsAsFactors = FALSE)

    x <- incidence(dat, date_index = "DATES", interval = 3, groups = "groups")
    y <- incidence(dat, date_index = "DATES", interval = 3)

    e1 <- estimate_peak(x)
    e2 <- estimate_peak(y)
    expect_named(
        e1,
        c("groups", "count_variable", "observed_peak", "observed_count",
          "bootstrap_peaks", "lower_ci", "median", "upper_ci")
    )
    expect_named(
        e2,
        c("count_variable", "observed_peak", "observed_count", "bootstrap_peaks", "lower_ci",
          "median", "upper_ci")
    )

    # The observed is identical to keap_peaks
    tmp <- keep_peaks(y, first_only = TRUE)
    expect_equal(e2$observed_peak, tmp[[1L]])
    expect_equal(e2$observed_count, tmp[[3L]])

    # The number of peaks defaults to 100
    expect_identical(nrow(e1$bootstrap_peaks[[1]]), 100L)
    expect_identical(nrow(e2$bootstrap_peaks[[1]]), 100L)

    # The observed falls within the confidence interval
    expect_gte(as.integer(e1$observed_peak[1]), as.integer(e1$lower_ci[1]))
    expect_lte(as.integer(e1$observed_peak[1]), as.integer(e1$upper_ci[1]))
    expect_gte(as.integer(e2$observed_peak[1]), as.integer(e2$lower_ci[1]))
    expect_lte(as.integer(e2$observed_peak[1]), as.integer(e2$upper_ci[1]))
})
