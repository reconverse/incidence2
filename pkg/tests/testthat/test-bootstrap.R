test_that("Fails with error", {
    expect_error(bootstrap_incidence(mtcars))
})

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
