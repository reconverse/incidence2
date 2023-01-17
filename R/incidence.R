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
#' The time index(es) of the given data.
#'
#' This should be the name(s) corresponding to the desired date column(s) in x.
#'
#' A name vector can be used for convenient relabelling of the resultant output.
#'
#' Multiple indices only make sense when  `x` is a linelist.
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
#' @param date_names_to `[character]`
#'
#' The name of the column to store the date variables in.
#'
#' @param rm_na_dates `[logical]`
#'
#' Should `NA` dates be removed prior to aggregation?
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
#'
#' @export
incidence <- function(
    x,
    date_index,
    groups = NULL,
    counts = NULL,
    count_names_to = "count_variable",
    count_values_to = "count",
    date_names_to = "date_index",
    rm_na_dates = TRUE,
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

    # date_names_to check
    .assert_scalar_character(date_names_to)

    # group checks
    if (!(is.null(groups) || is.character(groups)))
        stopf("`groups` must be NULL or a character vector.")

    if (!all(groups %in% names(x)))
        stopf("Not all variables from `groups` are present in `x`.")

    # boolean checks
    .assert_bool(rm_na_dates)

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

    # can we use data.table (cannot for vctrs_rcrd objects)
    use_dt <- !any(vapply(x, typeof, character(1)) == "list")

    if (isTRUE(use_dt)) {

        # Ensure we don't alter input
        DT <- as.data.table(x)

        # switch behaviour depending on if counts are already present
        if (is.null(counts)) {

            # make from wide to long
            DT <- melt(DT, measure.vars = date_index, variable.name = count_names_to, value.name = date_names_to, variable.factor = FALSE)

            # filter out NA dates if desired
            if (rm_na_dates) {
                na_id <- is.na(.subset2(DT, date_names_to))
                DT <- DT[!na_id]
            }

            res <- DT[, .N, keyby = c(date_names_to, groups, count_names_to)]
            setnames(res, length(res), count_values_to)
        } else {
            DT <- DT[, lapply(.SD, sum, na.rm = FALSE), keyby = c(date_index, groups), .SDcols = counts]
            res <- melt(DT, measure.vars = counts, variable.name = count_names_to, value.name = count_values_to)
            setnames(res, date_index, date_names_to)
        }

        # ensure we are nicely ordered
        setorderv(res, c(date_names_to, groups, count_names_to))

        # convert back to data frame
        setDF(res)
    } else {

        # switch behaviour depending on if counts are already present
        if (is.null(counts)) {

            # make from wide to long
            res <- pivot_longer(x, cols = date_index, names_to = count_names_to, values_to = date_names_to)

            # filter out NA dates if desired
            if (rm_na_dates) {
                na_id <- is.na(.subset2(res, date_names_to))
                res <- res[!na_id, , drop = FALSE]
            }

            vars <- c(date_names_to, groups, count_names_to)
            res <- count(res,across(all_of(vars)), name = count_values_to)
        } else {
            vars <- c(date_index, groups)
            res <- grouped_df(x, vars)
            res <- summarise(res, across(all_of(counts), sum, na.rm = FALSE), .groups = "drop")
            res <- pivot_longer(res, cols = all_of(counts), names_to = count_names_to, values_to = count_values_to)
            setnames(res, date_index, date_names_to)
            tmp <- .subset(res, c(date_names_to, groups, count_names_to))
            res <- res[do.call(order, unname(tmp)), , drop = FALSE]
        }

    }

    # if no groups set to character(0L)
    if (is.null(groups))
        groups <- character(0L)

    # return incidence object
    structure(
        res,
        date_index = date_names_to,
        count_variable = count_names_to,
        count_value = count_values_to,
        groups = groups,
        class = c("incidence", "data.frame")
    )
}
