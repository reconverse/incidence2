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
#' If `expand` is TRUE (default) then complete_dates will attempt to use
#' `function(x) seq(min(x), max(x), by = 1)` to generate a complete sequence of
#' dates.
#'
#'
#' @param by `[Defunct]`
#'
#' Ignored.
#'
#' @param fill `[numeric]`
#'
#' The value to replace missing counts by.
#'
#' Defaults to `0L`.
#'
#' @param allow_POSIXct `[logical]`
#'
#' Should this function work with POSIXct dates?
#'
#' Defaults to `FALSE`.
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
complete_dates <- function(
    x,
    expand = TRUE,
    fill = 0L,
    by = 1L,
    allow_POSIXct = FALSE
) {

    if (!inherits(x, "incidence2"))
        stopf("`%s` is not an 'incidence2' object", deparse(substitute(x)))

    .assert_bool(expand)

    if (length(fill) != 1L)
        stopf("`fill` must be of lenth 1.")

    date_variable <- get_date_index_name.incidence2(x)
    dates <- get_date_index.incidence2(x)

    # Note the following was motivated by
    # https://github.com/reconverse/incidence2/issues/104
    .assert_bool(allow_POSIXct)
    if (inherits(dates, "POSIXct") && !allow_POSIXct) {
        stopf(paste0(
            "<POSIXct> date_index columns detected. Internally <POSIXct> objects ",
            "are represented as seconds since the UNIX epoch and calling ",
            "`complete_dates()` on an object of granularity can lead to ",
            "significant memory usage. If you are sure you wish to do this, ",
            "please call again with the argument `allow_POSIXct` set to `TRUE`. ",
            "If this level of aggregation is not desired, consider recreating the ",
            "<incidence> object using <Dates> for daily incidence. This can be done ",
            "prior to calling `incidence()` or, alternatively, by setting the ",
            "argument `interval = 'day'` within the call itself."
        ))
    }

    if (by != 1)
        stopf("`by` argument is now Defunct. Setting `by = 1L` or `by = 1` is permitted for compatibility only.")

    # TODO - catch this and give better / combined error message
    if (expand)
        dates <- seq(min(dates), max(dates), by = 1L)
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
