#' Keep first, last and peak occurences
#'
#' `keep_first()` and `keep_last()` keep the first and last `n` rows to occur
#' for each grouping when in ascending date order. `keep_peaks()` keeps the rows
#' with the maximum count value for each group.
#'
# -------------------------------------------------------------------------
#' @param x `<incidence2>` object.
#'
#' @param n `[integer]`
#'
#' Number of entries to keep.
#'
#' `double` vectors will be converted via `as.integer(n)`.
#'
#' @param complete_dates `[bool]`
#'
#' Should `complete_dates()` be called on the data prior to keeping the first
#' entries.
#'
#' Defaults to TRUE.
#'
#' @param first_only `[bool]`
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
#' Incidence object with the chosen entries.
#'
# -------------------------------------------------------------------------
#' @examples
#'
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#' \dontshow{withAutoprint(\{}
#'     data(ebola_sim_clean, package = "outbreaks")
#'     dat <- ebola_sim_clean$linelist
#'     inci <- incidence(dat, "date_of_onset")
#'     keep_first(inci, 3)
#'     keep_last(inci, 3)
#' \dontshow{\})}
#' }
#'
# -------------------------------------------------------------------------
#' @name keep
NULL

#' @rdname keep
#' @export
keep_first <- function(x, n, complete_dates = TRUE, ...) {

    if (!inherits(x, "incidence2"))
        stopf("`x` must be an incidence2 object.")

    if (!is.integer(n)) {
        if (is.vector(n, "double")) {
            n <- as.integer(n)
        } else {
            stopf("`n` must be integer.")
        }
    }
    if (n <= 0L)
        stopf("`n` must be non-negative")

    .assert_bool(complete_dates)
    if (complete_dates)
        x <- complete_dates(x, ...)

    # pull out grouping variables
    groups <- get_group_names.incidence2(x)
    count_var <- get_count_variable_name.incidence2(x)

    # avoid check warnings
    tmp___index <- NULL

    # convert to data.table and order by date
    tmp <- as.data.table(x)
    tmp[, tmp___index := .I]
    setorderv(tmp, get_date_index_name.incidence2(x), order = 1L)

    # pull out n entries for each group
    tmp <- tmp[, list(tmp___index = head(tmp___index, n)), by = c(count_var, groups)]
    idx <- tmp$tmp___index

    # index input
    x[idx, ]
}

#' @rdname keep
#' @export
keep_last <- function(x, n, complete_dates = TRUE, ...) {

    if (!inherits(x, "incidence2"))
        stopf("`x` must be an incidence2 object.")

    if (!is.integer(n)) {
        if (is.vector(n, "double")) {
            n <- as.integer(n)
        } else {
            stopf("`n` must be integer.")
        }
    }
    if (n <= 0L)
        stopf("`n` must be non-negative")

    .assert_bool(complete_dates)
    if (complete_dates)
        x <- complete_dates(x, ...)

    # pull out grouping variables
    groups <- get_group_names.incidence2(x)
    count_var <- get_count_variable_name.incidence2(x)

    # avoid check warnings
    tmp___index <- NULL

    # convert to data.table and order by date
    tmp <- as.data.table(x)
    tmp[, tmp___index := .I]
    setorderv(tmp, get_date_index_name.incidence2(x))

    # pull out n entries for each group
    tmp <- tmp[, list(tmp___index = tail(tmp___index, n)), by = c(count_var, groups)]
    idx <- tmp$tmp___index

    # index input
    x[idx, ]
}

#' @rdname keep
#' @export
keep_peaks <- function(x, complete_dates = TRUE, first_only = FALSE, ...) {

    if (!inherits(x, "incidence2"))
        stopf("`x` must be an incidence2 object.")

    .assert_bool(complete_dates)
    if (complete_dates)
        x <- complete_dates(x, ...)

    .assert_bool(first_only)

    # pull out grouping variables
    groups <- get_group_names.incidence2(x)
    count_var <- get_count_variable_name.incidence2(x)

    # pull out date and count value variable
    count_value <- get_count_value_name.incidence2(x)
    date_var <- get_date_index_name.incidence2(x)
    date_values <- get_date_index.incidence2(x)[[1L]]

    # convert to data.table
    tmp <- as.data.table(x)

    # avoid check warnings
    tmp___index <- NULL

    # order by count
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

