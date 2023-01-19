#' Complete counts for all group combinations
#'
#' This function ensures that an incidence object has the same range of dates
#' for each grouping. By default missing counts will be filled with `0L`.
#'
#' @param x An `[incidence]` object.
#'
#' @param expand_dates `[logical]`
#'
#' Should a range of dates from the minimum to maximum value of the date index
#' also be created.
#'
#' @param by
#'
#' Passed as the `by` argument to seq.
#'
#' If `expand_dates` is TRUE (default) then complete counts will attempt to use
#' `function(x) seq(min(x), max(x), by = by)` to generate a complete sequence of
#' dates.
#'
#' Defaults to `1L`.
#'
#' @param fill `[numeric]`
#'
#' The value to replace missing counts by. Defaults to `0L`.
#'
#' @note
#' if `expand_dates` is TRUE (default) then complete counts will attempt to use
#' `function(x) seq(min(x), max(x), by = by)` to generate a complete sequence of
#' dates.
#'
#' @examples
#' x <- data.frame(
#'     dates = Sys.Date() + c(1,3,4),
#'     groups = c("grp1","grp2", "grp1"),
#'     counts = 1:3
#' )
#'
#' i <- incidence(x, date_index = "dates", groups = "groups", counts = "counts")
#' complete_counts(i)
#'
#' @export
complete_counts <- function(x, expand_dates = TRUE, fill = 0L, by = 1L) {

    if (!inherits(x, "incidence"))
        stopf("`%s` is not an 'incidence' object", deparse(substitute(x)))

    .assert_bool(expand_dates)

    if (length(fill) != 1L)
        stopf("`fill` must be of lenth 1.")

    date_variable <- get_date_index_name.incidence(x)
    dates <- get_date_index.incidence(x)[[1L]]
    # TODO - catch this and give better / combined error message
    if (expand_dates)
        dates <- seq(min(dates), max(dates), by = by)
    dates <- list(dates)
    names(dates) <- date_variable

    count_variable <- get_count_variable_name.incidence(x)
    counts <- get_count_variable.incidence(x)

    group_variables <- get_group_names.incidence(x)
    groups <- get_groups.incidence(x)

    groups <- c(dates, groups, counts)
    groups <- lapply(groups, unique)

    dat <- do.call(CJ, groups)
    out <- as.data.table(x)
    out <- as.data.frame(merge(dat, out, by = c(date_variable, group_variables, count_variable), all.x = TRUE))

    tmp <- .set_row_names(nrow(out))
    attributes(out) <- attributes(x)
    attr(out,"row.names") <- tmp
    setnafill(out, fill = fill, cols = get_count_value_name.incidence(x))
    out
}
