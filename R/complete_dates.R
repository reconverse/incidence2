#' Complete dates for all group combinations
#'
#' This function ensures that an incidence object has the same range of dates
#' for each grouping. By default missing counts will be filled with `0L`.
#'
# -------------------------------------------------------------------------
#' @param x `<incidence2>` object.
#'
#' @param expand `[logical]`
#'
#' Should a range of dates from the minimum to maximum value of the date index
#' also be created.
#'
#' @param by
#'
#' Passed as the `by` argument to seq.
#'
#' If `expand` is TRUE (default) then complete_dates will attempt to use
#' `function(x) seq(min(x), max(x), by = by)` to generate a complete sequence of
#' dates.
#'
#' Defaults to `1L`.
#'
#' @param fill `[numeric]`
#'
#' The value to replace missing counts by. Defaults to `0L`.
#'
# -------------------------------------------------------------------------
#' @return
#' An `<incidence2>` object.
#'
# -------------------------------------------------------------------------
#' @examples
#' x <- data.frame(
#'     dates = Sys.Date() + c(1,3,4),
#'     groups = c("grp1","grp2", "grp1"),
#'     counts = 1:3
#' )
#'
#' i <- incidence(x, date_index = "dates", groups = "groups", counts = "counts")
#' complete_dates(i)
#'
# -------------------------------------------------------------------------
#' @export
complete_dates <- function(x, expand = TRUE, fill = 0L, by = 1L) {

    if (!inherits(x, "incidence2"))
        stopf("`%s` is not an 'incidence2' object", deparse(substitute(x)))

    .assert_bool(expand)

    if (length(fill) != 1L)
        stopf("`fill` must be of lenth 1.")

    date_variable <- get_date_index_name.incidence2(x)
    dates <- get_date_index.incidence2(x)
    # TODO - catch this and give better / combined error message
    if (expand)
        dates <- seq(min(dates), max(dates), by = by)
    dates <- list(dates)
    names(dates) <- date_variable

    count_variable <- get_count_variable_name.incidence2(x)
    counts <- .subset(x, count_variable)

    group_variables <- get_group_names.incidence2(x)
    groups <- get_groups.incidence2(x)

    groups <- c(dates, groups, counts)
    groups <- lapply(groups, unique)

    dat <- do.call(CJ, groups)
    out <- as.data.table(x)
    out <- as.data.frame(merge(dat, out, by = c(date_variable, group_variables, count_variable), all.x = TRUE))

    tmp <- .set_row_names(nrow(out))
    attributes(out) <- attributes(x)
    attr(out,"row.names") <- tmp
    setnafill(out, fill = fill, cols = get_count_value_name.incidence2(x))
    out
}
