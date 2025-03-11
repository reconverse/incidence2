test_that("Default dispatch gives error", {
    expect_error(get_count_value(mtcars))
    expect_error(get_count_value_name(mtcars))
    expect_error(get_count_variable(mtcars))
    expect_error(get_count_variable_name(mtcars))
    expect_error(get_date_index(mtcars))
    expect_error(get_date_index_name(mtcars))
    expect_error(get_group_names(mtcars))
    expect_error(get_groups(mtcars))
})

test_that("Default dispatch gives good error message", {
    expect_snapshot(error = TRUE, get_count_value(mtcars))
    expect_snapshot(error = TRUE, get_count_value_name(mtcars))
    expect_snapshot(error = TRUE, get_count_variable(mtcars))
    expect_snapshot(error = TRUE, get_count_variable_name(mtcars))
    expect_snapshot(error = TRUE, get_date_index(mtcars))
    expect_snapshot(error = TRUE, get_date_index_name(mtcars))
    expect_snapshot(error = TRUE, get_group_names(mtcars))
    expect_snapshot(error = TRUE, get_groups(mtcars))
})

test_that("Accessors work as expected", {
    skip_if_not_installed("outbreaks")
    dat <- outbreaks::ebola_sim_clean$linelist
    i <- incidence(
        dat,
        date_index = "date_of_onset",
        groups = c("gender", "hospital"),
        count_names_to = "cv",
        count_values_to = "co",
        date_names_to = "di"
    )
    expect_identical(get_count_value(i), .subset2(i, "co"))
    expect_identical(get_count_value_name(i), "co")
    expect_identical(get_count_variable(i), .subset2(i, "cv"))
    expect_identical(get_count_variable_name(i), "cv")
    expect_identical(get_date_index(i), .subset2(i, "di"))
    expect_identical(get_date_index_name(i), "di")
    expect_identical(get_group_names(i), c("gender", "hospital"))
    expect_identical(get_groups(i), .subset(i, c("gender", "hospital")))
})
