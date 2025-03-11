test_that("defunct functions are defunct!", {
    expect_error(new_incidence(mtcars))
    expect_error(validate_incidence(mtcars))
    expect_error(build_incidence(mtcars))
    expect_error(get_n(mtcars))
    expect_error(get_interval(mtcars))
    expect_error(get_timespan(mtcars))
    expect_error(facet_plot(mtcars))
})

test_that("defunct functions give good error messaging!", {
    expect_snapshot(error = TRUE, new_incidence(mtcars), cnd_class = TRUE)
    expect_snapshot(error = TRUE, validate_incidence(mtcars), cnd_class = TRUE)
    expect_snapshot(error = TRUE, build_incidence(mtcars), cnd_class = TRUE)
    expect_snapshot(error = TRUE, get_n(mtcars), cnd_class = TRUE)
    expect_snapshot(error = TRUE, get_interval(mtcars), cnd_class = TRUE)
    expect_snapshot(error = TRUE, get_timespan(mtcars), cnd_class = TRUE)
    expect_snapshot(error = TRUE, facet_plot(mtcars), cnd_class = TRUE)
})
