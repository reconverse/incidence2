test_that("cumulate works with groupings", {
    int <- sample(-3L:50L, 100, replace = TRUE)
    dates <- as.Date("2018-01-31") + int
    group_1 <- sample(letters[1:3], length(dates), replace = TRUE)
    group_2 <- sample(letters[1:3], length(dates), replace = TRUE)
    dat <- data.frame(dates, group_1, group_2)
    dat$dates <- as_period(dat$dates, n = 14L)
    x <- incidence(dat, date_index = "dates", groups = "group_1")

    xc <- x
    xc_dates <- seq(min(xc$date_index), max(xc$date_index))
    tmp <- expand.grid(date_index = xc_dates, group_1 = unique(group_1))
    tmp2 <- merge(tmp, xc, by = c("date_index", "group_1"), all.x = TRUE)
    tmp2[,"count"][is.na(tmp2[,"count"])] <- 0
    tmp2$count_variable <- "dates"
    tmp2$group_1 <- as.character(tmp2$group_1)
    for (gr in unique(group_1)) {
        idx <- tmp2$group_1 == gr
        tmp2[idx, "count"] <- cumsum(tmp2[idx, "count"])
    }

    res <- cumulate(complete_dates(x))
    res <- res[do.call(order, as.list(res)), ]
    tmp2 <- tmp2[do.call(order, as.list(tmp2)), ]

    attributes(tmp2) <- NULL
    attributes(res) <- NULL

    expect_equal(list2DF(res), list2DF(tmp2))

})

test_that("cumulate works without groupings", {
    dates <- as.Date("2018-01-31") + 1:100
    dat <- data.frame(dates)
    x <- incidence(dat, date_index = "dates")
    expected_count <- cumsum(rep(1,nrow(dat)))
    expect_equal(cumulate(x)$cumulative_count, expected_count)
})

