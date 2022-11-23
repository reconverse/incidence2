dates <- Sys.Date() + 1:10
x <- incidence(data.frame(dates), date_index = "dates")

# as.data.frame
expected <- data.frame(date_index = dates, count_variable = "dates", count = 1L)
expect_identical(as.data.frame(x), expected)
