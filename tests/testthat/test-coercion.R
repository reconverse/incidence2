test_that("as.data.frame coercion works", {
    dates <- Sys.Date() + 1:10
    expected <- data.frame(date_index = dates, count_variable = "dates", count = 1L)
    x <- incidence(data.frame(dates), date_index = "dates")
    expect_identical(as.data.frame(x), expected)
})
