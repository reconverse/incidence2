#' Keep first, last and peak occurences
#'
#' `keep_first()` and `keep_last()` keep the first and last `n` rows to occur
#' for each grouping when in ascending date order. `keep_peaks()` keeps the rows
#' with the maximum count value for each group. `first_peak()` is a convenience
#' wrapper around `keep_peaks()` with the `first_only` argument set to `TRUE`.
#'
# -------------------------------------------------------------------------
#' @param x [incidence2][incidence2::incidence] object.
#'
#' @param n `integer`.
#'
#' Number of entries to keep.
#'
#' `double` vectors will be converted via `as.integer(n)`.
#'
#' @param complete_dates `bool`.
#'
#' Should `complete_dates()` be called on the data prior to keeping the first
#' entries.
#'
#' Defaults to TRUE.
#'
#' @param first_only `bool`.
#'
#' Should only the first peak (by date) be kept.
#'
#' Defaults to `TRUE`.
#'
#' @param ...
#'
#' Other arguments passed to `complete_dates()`.
#'
# -------------------------------------------------------------------------
#' @return
#' [incidence2][incidence2::incidence] object with the chosen entries.
#'
# -------------------------------------------------------------------------
#' @examples
#' \dontshow{.old <- data.table::setDTthreads(2)}
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#'     data(ebola_sim_clean, package = "outbreaks")
#'     dat <- ebola_sim_clean$linelist
#'     inci <- incidence(dat, "date_of_onset")
#'     keep_first(inci, 3)
#'     keep_last(inci, 3)
#' }
#' \dontshow{data.table::setDTthreads(.old)}
#'
# -------------------------------------------------------------------------
#' @name keep
NULL

#' @rdname keep
#' @export
keep_first <- function(x, n, complete_dates = TRUE, ...) {

    if (!inherits(x, "incidence2"))
        stop("`x` must be an incidence2 object.")

    assert_scalar_whole(n)
    if (n <= 0L)
        stop("`n` must be non-negative")

    assert_bool(complete_dates)
    if (complete_dates)
        x <- complete_dates(x, ...)

    # pull out grouping variables
    groups <- get_group_names.incidence2(x)
    count_var <- get_count_variable_name.incidence2(x)

    # avoid check warnings
    tmp___index <- NULL

    # convert to data.table and order by date
    if ("tmp___index" %in% names(x)) {
        .stop(
            "`keep_first` does not work if a column is already named 'tmp___index'. ",
            "Please rename and try again. ",
            "If this is problematic, please raise an issue."
        )
    }
    tmp <- as.data.table(x)
    tmp[, tmp___index := .I]
    setorderv(tmp, get_date_index_name.incidence2(x), order = 1L)

    # pull out n entries for each group
    tmp <- tmp[, list(tmp___index = utils::head(tmp___index, n)), by = c(count_var, groups)]
    idx <- tmp$tmp___index

    # index input
    x[idx, ]
}

#' @rdname keep
#' @export
keep_last <- function(x, n, complete_dates = TRUE, ...) {

    if (!inherits(x, "incidence2"))
        stop("`x` must be an incidence2 object.")

    assert_scalar_whole(n)
    if (n <= 0L)
        stop("`n` must be non-negative")

    assert_bool(complete_dates)
    if (complete_dates)
        x <- complete_dates(x, ...)

    # pull out grouping variables
    groups <- get_group_names.incidence2(x)
    count_var <- get_count_variable_name.incidence2(x)

    # avoid check warnings
    tmp___index <- NULL

    # convert to data.table and order by date
    if ("tmp___index" %in% names(x)) {
        .stop(
            "`keep_last` does not work if a column is already named 'tmp___index'. ",
            "Please rename and try again. ",
            "If this is problematic, please raise an issue."
        )
    }
    tmp <- as.data.table(x)
    tmp[, tmp___index := .I]
    setorderv(tmp, get_date_index_name.incidence2(x))

    # pull out n entries for each group
    tmp <- tmp[, list(tmp___index = utils::tail(tmp___index, n)), by = c(count_var, groups)]
    idx <- tmp$tmp___index

    # index input
    x[idx, ]
}

#' @rdname keep
#' @export
keep_peaks <- function(x, complete_dates = TRUE, first_only = FALSE, ...) {

    if (!inherits(x, "incidence2"))
        stop("`x` must be an incidence2 object.")

    assert_bool(complete_dates)
    if (complete_dates)
        x <- complete_dates(x, ...)

    assert_bool(first_only)

    # pull out grouping variables
    groups <- get_group_names.incidence2(x)
    count_var <- get_count_variable_name.incidence2(x)

    # pull out date and count value variable
    count_value <- get_count_value_name.incidence2(x)
    date_var <- get_date_index_name.incidence2(x)
    date_values <- get_date_index.incidence2(x)

    # convert to data.table
    tmp <- as.data.table(x)

    # TODO - do this better
    # avoid check warnings
    tmp___index <- NULL

    # order by count
    if ("tmp___index" %in% names(x)) {
        .stop(
            "`keep_peaks` does not work if a column is already named 'tmp___index'. ",
            "Please rename and try again. ",
            "If this is problematic, please raise an issue."
        )
    }
    tmp[, tmp___index := .I]
    tmp <- tmp[,
               list(tmp___index = tmp___index[.SD == max(.SD)]),
               by = c(count_var, groups),
               .SDcols = count_value]
    tmp[, (date_var) := date_values[tmp$tmp___index]]

    # do we want to keep only the first peak
    if (first_only) {
        tmp <- tmp[,
                   list(tmp___index = tmp___index[which.min(.SD[[1L]])]),
                   by = c(count_var, groups),
                   .SDcols = date_var]
    }

    # index input
    idx <- tmp$tmp___index
    x[idx, ]
}

#' @rdname keep
#' @export
first_peak <- function(x, complete_dates = TRUE, ...) {
    keep_peaks(x = x, complete_dates = complete_dates, first_only = TRUE, ...)
}
