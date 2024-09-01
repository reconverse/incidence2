test_that("Fails with good error", {
    blobby <- mtcars
    expect_snapshot(error = TRUE, bootstrap_incidence(blobby))
})

test_that("bootstrap_incidence works as expected", {
    # no idea why I'm currently skipping this on CRAN (threads?)
    skip_on_cran()

    skip_if_not_installed("withr")
    withr::with_seed(seed = 1, {
        dates <- as.integer(sample(-3:100, 50, replace = TRUE))
        groups <- sample(c("toto", "tata"), 50, replace = TRUE)
    })
    DATES <- as.Date("2016-09-20") + dates
    dat <- data.frame(DATES, groups = groups, stringsAsFactors = FALSE)

    x <- incidence(dat, date_index = "DATES", interval = 3, groups = "groups")
    y <- bootstrap_incidence(x)
    z <- bootstrap_incidence(x, randomise_groups = TRUE)

    expect_identical(sum(x$count), sum(y$count))
    expect_identical(sum(x$count), sum(z$count))
    expect_identical(names(x), names(y))
    expect_identical(names(x), names(z))
    expect_true(setequal(y$groups, z$groups))
})



#
# # test_that("Bootstrap needs an incidence object", {
# #     expect_error(bootstrap_incidence(DATES), "`DATES` is not an 'incidence2' object")
# # })
#
#
# test_that("estimate_peak needs an incidence object", {
#     expect_error(estimate_peak(DATES), "`DATES` is not an 'incidence2' object")
# })
#
#

#
#
# test_that("estimate_peak can roughly estimate it", {
#
#     x <- incidence(dat, date_index = "DATES", interval = 3, groups = "groups")
#     y <- incidence(dat, date_index = "DATES", interval = 3)
#
#     e1 <- estimate_peak(x)
#     e2 <- estimate_peak(y)
#     expect_named(
#         e1,
#         c("groups", "count_variable", "observed_peak", "observed_count",
#           "bootstrap_peaks", "lower_ci", "median", "upper_ci")
#     )
#     expect_named(
#         e2,
#         c("count_variable", "observed_peak", "observed_count", "bootstrap_peaks", "lower_ci",
#           "median", "upper_ci")
#     )
#
#     # The observed is identical to keap_peaks
#     tmp <- keep_peaks(y, first_only = TRUE)
#     expect_equal(e2$observed_peak, tmp[[1L]])
#     expect_equal(e2$observed_count, tmp[[3L]])
#
#     # The number of peaks defaults to 100
#     expect_identical(nrow(e1$bootstrap_peaks[[1]]), 100L)
#     expect_identical(nrow(e2$bootstrap_peaks[[1]]), 100L)
#
#     # The observed falls within the confidence interval
#     expect_gte(as.integer(e1$observed_peak[1]), as.integer(e1$lower_ci[1]))
#     expect_lte(as.integer(e1$observed_peak[1]), as.integer(e1$upper_ci[1]))
#     expect_gte(as.integer(e2$observed_peak[1]), as.integer(e2$lower_ci[1]))
#     expect_lte(as.integer(e2$observed_peak[1]), as.integer(e2$upper_ci[1]))
# })
