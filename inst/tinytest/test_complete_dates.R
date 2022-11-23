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
expect_identical(sum(res2$counts1), sum(i$counts1))
expect_identical(sum(res2$counts2), sum(i$counts2))
expect_identical(nrow(res2), 32L)



