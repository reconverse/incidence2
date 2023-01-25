#' Keep first and last occurences
#'
#' `keep_first()` (`keep_last`) keeps the first (last) `n` rows to occur for
#' each group when in ascending date order.
#'
#' @param x `[incidence2]` object.
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
#' @param ...
#'
#' Other arguments passed to `complete_dates()`.
#'
#' @return
#'
#' Incidence object with the chosen entries.
#'
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

    # convert to data.table and order by date
    tmp <- as.data.table(x)
    setorderv(tmp, get_date_index_name.incidence2(x))

    # pull out n entries for each group
    tmp <- tmp[, list(tmp___123 = head(.I, n)), by = c(count_var, groups)]
    idx <- tmp$tmp___123

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

    # convert to data.table and order by date
    tmp <- as.data.table(x)
    setorderv(tmp, get_date_index_name.incidence2(x))

    # pull out n entries for each group
    tmp <- tmp[, list(tmp___123 = tail(.I, n)), by = c(count_var, groups)]
    idx <- tmp$tmp___123

    # index input
    x[idx, ]
}
