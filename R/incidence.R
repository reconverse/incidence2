#' Compute the incidence of events
#'
#' `incidence()` calculates event the *incidence* of different events across
#' specified time periods and groupings.
#'
#' @param x
#'
#' A data frame object representing a linelist or pre-aggregated dataset.
#'
#' @param date_index `[character]`
#'
#' The time index(es) of the given data.  This should be the name(s)
#' corresponding to the desired date column(s) in x. Multiple indices only make
#' sense when  `x` is a linelist; in this situation the vector must be named and
#' these names will be used for the resultant count variables.
#'
#' @param groups `[character]`
#'
#' An optional vector giving the names of the groups of observations for which
#' incidence should be grouped.
#'
#' @param counts `[character]`
#'
#' The count variables of the given data.  If NULL (default) the data is taken
#' to be a linelist of individual observations.
#'
#' @param count_names_to `[character]`
#'
#' The column to create which will store the `counts` column names provided that
#' `counts` is not NULL.
#'
#' @param count_values_to `[character]`
#'
#' The name of the column to store the resultant count values in.
#'
#' @param rm_na_dates `[logical]`
#'
#' Should `NA` dates be removed prior to aggregation?
#'
#' @param na_as_zero `[logical]`
#'
#' Should explicitly missing `NA` counts be treated as zero?
#'
#' This can have multiple effects depending on whether we are aggregating over
#' multiple date indices whether the input `x` is pre-aggregated, with specified
#' `count` variables, or, a linelist:
#'
#' - If `date_index` has length greater than one, it is possible for some
#'   groupings to not occur for particular date variables. When merging the
#'   resulting aggregated tables the missing group counts will initially be set
#'   to `NA`. If `na_as_zero` is `TRUE` they will then be converted to 0.
#'
#' - If `x` is pre-aggregated, with counts specified, then setting `na_as_zero`
#'   to `TRUE` replaces all `NA` within the `count` columns to 0, prior to
#'   aggregation.
#'
#' @param ... Not currently used
#'
#' @details
#'
#' `<incidence>` objects are a sub class of data frame with some
#' additional invariants. That is, an `<incidence>` object must:
#'
#' - have one column representing the date index (this does not need to be a
#'   `date` object but must have an inherent ordering over time);
#'
#' - have one column representing the count variable (i.e. what is being
#'   counted) and one variable representing the associated count;
#'
#' - have zero or more columns representing groups;
#'
#' - not have duplicated rows with regards to the date and group variables.
#'
#' @return
#'
#' An object of class `[incidence, data.frame]`.
#'
#' @examples
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#' \dontshow{withAutoprint(\{}
#'     data(ebola_sim_clean, package = "outbreaks")
#'     dat <- ebola_sim_clean$linelist
#'     incidence(dat, "date_of_onset")
#'     incidence(dat, "date_of_onset", groups = c("gender", "hospital"))
#' \dontshow{\})}
#' }
#'
#' @export
incidence <- function(
    x,
    date_index,
    groups = NULL,
    counts = NULL,
    count_names_to = "count_variable",
    count_values_to = "count",
    rm_na_dates = TRUE,
    na_as_zero = TRUE,
    ...
) {

    if (!is.data.frame(x))
        stopf("`x` must be a dataframe.")

    # date_index checks
    length_date_index <- length(date_index)

    if (!(is.character(date_index) && length_date_index))
        stopf("`date_index` must be a character vector of length 1 or more.")

    if (!all(date_index %in% names(x)))
        stopf("Not all variables from `date_index` are present in `x`.")

    date_cols <- .subset(x, date_index)
    date_classes <- vapply(date_cols, function(x) class(x)[1], "")
    if (length(unique(date_classes)) != 1L)
        stopf("`date_index` columns must be of the same class.")

    # TODO - remove this once dplyr support added
    is_list_date <- vapply(date_cols, function(x) inherits(x, c("POSIXlt", "vctrs_rcrd")), TRUE)
    if (any(is_list_date))
        stopf("vctrs_rcrd and POSIXlt date_index columns are not currently supported.")

    # counts checks
    if (!is.null(counts)) {
        if (!is.character(counts) || length(counts) < 1L)
            stopf("`counts` must be NULL or a character vector of length 1 or more.")

        if (length_date_index > 1)
            stopf("If `counts` is specified `date_index` must be of length 1.")
    }

    if (!all(counts %in% names(x)))
        stopf("Not all variables from `counts` are present in `x`.")

    # count_names_to check
    .assert_scalar_character(count_names_to)

    # count_values_to check
    .assert_scalar_character(count_values_to)

    # group checks
    if (!(is.null(groups) || is.character(groups)))
        stopf("`groups` must be NULL or a character vector.")

    # TODO - remove this once dplyr support added
    if (length(groups)) {
        group_cols <- .subset(x, groups)
        is_list_group <- vapply(group_cols, function(x) inherits(x, c("POSIXlt", "vctrs_rcrd")), TRUE)
        if (any(is_list_group))
            stopf("vctrs_rcrd and POSIXlt date_index columns are not currently supported.")
    }

    if (!all(groups %in% names(x)))
        stopf("Not all variables from `groups` are present in `x`.")

    # boolean checks
    .assert_bool(rm_na_dates)
    .assert_bool(na_as_zero)

    # convert to data.table
    x <- as.data.table(x)

    # generate name for date_index column
    nms <- names(date_index)
    if (!is.null(nms)) {
        if (length_date_index == 1L && nms != "") {
            setnames(x, date_index, nms)
            date_index <- nms
        } else if (any(nms != "")) {
            new_names <- date_index
            new_names[nms != ""] <- nms
            setnames(x, date_index, new_names)
            date_index <- new_names
        }
    }

    # apply .single_date_incidence for each date_index
    res <- lapply(
        X = date_index,
        FUN = .single_date_incidence,
        DT = x,
        groups = groups,
        counts = counts,
        count_names_to = count_names_to,
        count_values_to = count_values_to,
        rm_na_dates = rm_na_dates,
        na_as_zero = na_as_zero
    )

    # if there is only 1 value for date_index we can just return the entry,
    # otherwise we need to merge the results and fill na values
    if (length_date_index == 1) {
        res <- res[[1L]]
    } else {
        res <- rbindlist(res)
        if (na_as_zero) {
            setnafill(res, fill = 0L, cols = count_values_to)
        }
    }

    # ensure we are nicely ordered
    setorderv(res, c("date_index", groups, count_names_to))

    # convert back to data frame
    setDF(res)

    # if no groups set to character(0L)
    if (is.null(groups))
        groups <- character(0L)

    .new_incidence(
        res,
        date_index = "date_index",
        count_variable = count_names_to,
        count_value = count_values_to,
        groups = groups
    )
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
.single_date_incidence <- function(
        DT,
        date_index,
        groups,
        counts,
        count_names_to,
        count_values_to,
        rm_na_dates,
        na_as_zero
) {

    # Ensure we don't alter input
    DT <- copy(DT)

    # filter out NA dates if desired
    if (rm_na_dates) {
        na_id <- is.na(.subset2(DT, date_index))
        DT <- DT[!na_id]
    }

    # switch behaviour depending on if counts are already present
    if (is.null(counts)) {
        # hacky use of count___123 but it works.
        DT <- DT[, list(count___123 = .N), keyby = c(date_index, groups)]
        DT <- melt(
            DT,
            measure.vars = "count___123",
            variable.name = count_names_to,
            value.name = count_values_to,
            variable.factor = FALSE
        )
        set(DT, j = count_names_to, value = date_index)
    } else {
        # filter out NA counts if desired
        if (rm_na_dates) {
            na_id <- is.na(.subset2(DT, date_index))
            DT <- DT[!na_id]
        }
        if (na_as_zero)
            setnafill(DT, fill = 0L, cols = counts)
        DT <- DT[, lapply(.SD, sum, na.rm = FALSE), keyby = c(date_index, groups), .SDcols = counts]
        DT <- melt(DT, measure.vars = counts, variable.name = count_names_to, value.name = count_values_to)
    }


    # give date column correct name and return columns in desired order
    setnames(DT, 1L, "date_index")
    vars <- c("date_index", groups, count_names_to)
    setorderv(DT, vars)
}


.new_incidence <- function(x, date_index, count_variable, count_value, groups) {
    structure(
        x,
        date_index = date_index,
        count_variable = count_variable,
        count_value = count_value,
        groups = groups,
        class = c("incidence", "data.frame")
    )
}
