#' Compute the incidence of events
#'
#' `incidence()` calculates event the *incidence* of different events across
#' specified time periods and groupings.
#'
# -------------------------------------------------------------------------
#' `<incidence2>` objects are a sub class of data frame with some
#' additional invariants. That is, an `<incidence2>` object must:
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
# -------------------------------------------------------------------------
#' # Interval specification
#'
#' Where `interval` is specified, `incidence()` uses the
#' [`grates`](https://cran.r-project.org/package=grates) package to generate
#' appropriate date groupings. The grouping used depends on the value of
#' `interval`. This can be specified as either an integer value or a string
#' corresponding to one of the grates classes:
#'
#' - integer values:                     [`<grates_period>`][grates::new_period] object, grouped by the specified number of days.
#' - week(s), weekly, isoweek:           [`<grates_isoweek>`][grates::isoweek] objects.
#' - epiweek(s):                         [`<grates_epiweek>`][grates::epiweek] objects.
#' - month(s), monthly, yearmonth:       [`<grates_yearmonth>`][grates::yearmonth] objects.
#' - quarter(s), quarterly, yearquarter: [`<grates_yearquarter>`][grates::yearquarter] objects.
#' - year(s) and yearly:                 [`<grates_year>`][grates::year] objects.
#'
# -------------------------------------------------------------------------
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
#' @param interval
#'
#' An optional scalar integer or string indicating the (fixed) size of
#' the desired time interval you wish to use for for computing the incidence.
#'
#' Defaults to NULL in which case the date_index columns are left unchanged.
#'
#' Numeric values are coerced to integer and treated as a number of days to
#' group.
#'
#' Text strings can be one of:
#'
#'     * week(s) or weekly
#'     * epiweek(s)
#'     * isoweek(s)
#'     * month(s) or monthly
#'     * yearmonth(s)
#'     * quarter(s) or quarterly
#'     * yearquarter(s)
#'     * year(s) or yearly
#'
#' More details can be found in the "Interval specification" section.
#'
#' @param offset
#'
#' Only applicable when `interval` is not NULL.
#'
#' An optional scalar integer or date indicating the value you wish to start
#' counting periods from relative to the Unix Epoch:
#'
#' - Default value of NULL corresponds to 0L.
#'
#' - For other integer values this is stored scaled by `n`
#'   (`offset <- as.integer(offset) %% n`).
#'
#' - For date values this is first converted to an integer offset
#'   (`offset <- floor(as.numeric(offset))`) and then scaled via `n` as above.
#'
#' @param ...
#'
#' Not currently used.
#'
# -------------------------------------------------------------------------
#' @seealso
#' `browseVignettes("grates")` for more details on the grate object classes.
#'
# -------------------------------------------------------------------------
#' @return
#' An object of class `<incidence2, data.frame>`.
#'
# -------------------------------------------------------------------------
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
# -------------------------------------------------------------------------
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
    interval = NULL,
    offset = NULL,
    ...
) {

    # handle defunct arguments
    if (...length()) {
        if (getRversion() >= "4.1.0") {
            nms <- ...names()
        } else {
            dots <- match.call(expand.dots = FALSE)$`...`
            nms <- names(dots)
        }
        idx <- nms %in% c("na_as_group", "firstdate")
        if(any(idx)) {
            nms <- nms[idx]
            stopf("As of incidence 2.0.0, `%s` is no longer a valid parameter name. See `help('incidence')` for supported parameters.", nms[1L])
        } else if (is.null(nms)) {
            stop("Too many arguments given.")
        } else {
            nms <- nms[nms != ""]
            stopf("`%s` is not a valid parameter", nms[1L])
        }

    }

    # x must be a data frame
    if (!is.data.frame(x))
        stopf("`x` must be a data frame.")

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

    # error if date_index cols are vctrs_rcrd type
    is_vctrs_rcrd <- sapply(date_cols, inherits, "vctrs_rcrd")
    if (any(is_vctrs_rcrd))
        stopf("vctrs_rcrd date_index columns are not currently supported.")

    # error if date_index cols are POSIXlt
    is_POSIXlt <- sapply(date_cols, inherits, "POSIXlt")
    if (any(is_POSIXlt))
        stopf("POSIXlt date_index columns are not currently supported.")

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

    if (length(groups)) {
        # ensure groups are present
        if (!all(groups %in% names(x)))
            stopf("Not all variables from `groups` are present in `x`.")

        # error if group cols are vctrs_rcrd type
        group_cols <- .subset(x, groups)
        is_vctrs_rcrd <- sapply(group_cols, inherits, "vctrs_rcrd")
        if (any(is_vctrs_rcrd))
            stopf("vctrs_rcrd group columns are not currently supported.")

        # error if group cols are POSIXlt
        is_POSIXlt <- sapply(group_cols, inherits, "POSIXlt")
        if (any(is_POSIXlt))
            stopf("POSIXlt group columns are not currently supported.")
    }

    # boolean checks
    .assert_bool(rm_na_dates)

    # check interval and apply transformation across date index
    if (!is.null(interval)) {

        # check interval is valid length
        if (length(interval) != 1L)
            stopf("`interval` must be a character or integer vector of length 1.")

        # For numeric we coerce to integer and use as_period
        if (is.numeric(interval)) {
            n <- as.integer(interval)

            # coerce offset (do here rather than in grates for better easier error messagin)
            if (!is.null(offset)) {
                if (inherits(offset, "Date"))
                    offset <- floor(as.numeric(offset))

                if (!.is_scalar_whole(offset))
                    stop("`offset` must be an integer or date of length 1.")
            } else {
                offset <- 0L
            }
            x[date_index] <- lapply(x[date_index], as_period, n = n, offset = offset)
        } else if (!is.null(offset)) {
            # offset only valid for numeric interval
            stopf("`offset` can only be used with a numeric (period) interval.")
        } else if (is.character(interval)) {
            # We are restrictive on intervals we allow to keep the code simple.
            # Users can always call grates functionality directly (reccomended)
            # and not use the `interval` argument which mainly a convenience
            # for new users and interactive work.
            interval <- tolower(interval)
            FUN <- switch(EXPR = interval,
                week        =,
                weeks       =,
                weekly      =,
                isoweek     =,
                isoweeks    = as_isoweek,
                epiweek     =,
                epiweeks    = as_epiweek,
                month       =,
                months      =,
                monthly     =,
                yearmonth   = as_yearmonth,
                quarter     =,
                quarters    =,
                quarterly   =,
                yearquarter = as_yearquarter,
                year        =,
                years       =,
                yearly       = as_year,
                stopf(paste(
                    "`interval` must be one of:",
                    "    - an <integer> value;",
                    "    - 'week(s)', 'weekly' or 'isoweek';",
                    "    - 'epiweek(s)';",
                    "    - 'month(s)', 'monthly', 'yearmonth';",
                    "    - 'quarter(s)', 'quarterly', 'yearquarter';",
                    "    - 'year(s)' or 'yearly'.",
                    sep = "\n"
                ))
            )
            x[date_index] <- lapply(x[date_index], FUN)
        } else {
            stopf("`interval` must be a character or integer vector of length 1.")
        }
    }

    # create data.table
    DT <- as.data.table(x)

    # generate name for date_index column (ensure coerced to DT before using setnames)
    nms <- names(date_index)
    if (!is.null(nms)) {
        if (length_date_index == 1L && nms != "") {
            setnames(DT, date_index, nms)
            date_index <- nms
        } else if (any(nms != "")) {
            new_names <- date_index
            new_names[nms != ""] <- nms
            setnames(DT, date_index, new_names)
            date_index <- new_names
        }
    }

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
        class = c("incidence2", "data.frame")
    )
}
