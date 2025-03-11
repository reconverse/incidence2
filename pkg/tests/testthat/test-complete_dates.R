test_that("Fails with error for bad input", {
    dat <- data.frame(
        dates = Sys.Date() + 1:4,
        groups1 = rep(c("bob","george"), 2),
        groups2 = c(rep("groupa", 2), rep("groupb", 2)),
        counts1 = 1:4,
        counts2 = 11:14
    )

    i <- incidence(
        dat,
        date_index = "dates",
        groups = c("groups1", "groups2"),
        counts = c("counts1", "counts2")
    )

    expect_error(complete_dates(dat))
    expect_error(complete_dates(i, expand = "bob"))
    expect_error(complete_dates(i, allow_POSIXct = "bob"))
    expect_error(complete_dates(i, fill = 1:2))
    expect_error(complete_dates(i, by = 2))

    i$date_index <- as.POSIXct(i$date_index)
    expect_error(complete_dates(i))
})

test_that("Fails with good error for bad input", {
    dat <- data.frame(
        dates = Sys.Date() + 1:4,
        groups1 = rep(c("bob","george"), 2),
        groups2 = c(rep("groupa", 2), rep("groupb", 2)),
        counts1 = 1:4,
        counts2 = 11:14
    )

    i <- incidence(
        dat,
        date_index = "dates",
        groups = c("groups1", "groups2"),
        counts = c("counts1", "counts2")
    )

    expect_snapshot(error = TRUE, complete_dates(dat))
    expect_snapshot(error = TRUE, complete_dates(i, expand = "bob"))
    expect_snapshot(error = TRUE, complete_dates(i, allow_POSIXct = "bob"))
    expect_snapshot(error = TRUE, complete_dates(i, fill = 1:2))
    expect_snapshot(error = TRUE, complete_dates(i, by = 2))

    i$date_index <- as.POSIXct(i$date_index)
    expect_snapshot(error = TRUE, complete_dates(i))
})

test_that("complete_dates works", {
    dat <- data.frame(
        dates = Sys.Date() + 1:4,
        groups1 = rep(c("bob","george"), 2),
        groups2 = c(rep("groupa", 2), rep("groupb", 2)),
        counts1 = 1:4,
        counts2 = 11:14
    )

    i <- incidence(
        dat,
        date_index = "dates",
        groups = c("groups1", "groups2"),
        counts = c("counts1", "counts2")
    )

    res <- complete_dates(i, fill = NA)
    expect_identical(nrow(res), 32L)
    tmp <- res[complete.cases(res),]
    row.names(tmp) <- NULL
    expect_identical(tmp, i)
    res2 <- complete_dates(i)

    expect_identical(
        tapply(res2$count, res2$count_variable, sum),
        tapply(i$count, i$count_variable, sum)
    )

    expect_identical(nrow(res2), 32L)

    expect_identical(
        incidence(
            dat,
            date_index = "dates",
            groups = c("groups1", "groups2"),
            counts = c("counts1", "counts2"),
            complete_dates = TRUE
        ),
        res2
    )
})
