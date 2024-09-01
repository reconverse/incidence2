test_that("defunct functions are defunct!", {
    expect_snapshot(error = TRUE, new_incidence(mtcars), cnd_class = TRUE)
    expect_snapshot(error = TRUE, validate_incidence(mtcars), cnd_class = TRUE)
    expect_snapshot(error = TRUE, build_incidence(mtcars), cnd_class = TRUE)
    expect_snapshot(error = TRUE, get_n(mtcars), cnd_class = TRUE)
    expect_snapshot(error = TRUE, get_interval(mtcars), cnd_class = TRUE)
    expect_snapshot(error = TRUE, get_timespan(mtcars), cnd_class = TRUE)
    expect_snapshot(error = TRUE, facet_plot(mtcars), cnd_class = TRUE)
})
