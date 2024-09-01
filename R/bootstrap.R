#' Bootstrap incidence time series
#'
# -------------------------------------------------------------------------
#' This function can be used to bootstrap [incidence2][incidence2::incidence]
#' objects. Bootstrapping is done by sampling with replacement the original
#' input dates.
#'
# -------------------------------------------------------------------------
#' As original data are not stored in [incidence2][incidence2::incidence]
#' objects, the bootstrapping is achieved by multinomial sampling of date bins
#' weighted by their relative incidence.
#'
# -------------------------------------------------------------------------
#' @param x An [incidence2][incidence2::incidence] object.
#'
#' @param randomise_groups `bool`.
#'
#' Should groups be randomised as well in the resampling procedure; respective
#' group sizes will be preserved, but this can be used to remove any
#' group-specific temporal dynamics.
#'
#' If `FALSE` (default), data are resampled within groups.
#'
# -------------------------------------------------------------------------
#' @return
#'
#' An [incidence2][incidence2::incidence] object.
#'
# -------------------------------------------------------------------------
#' @author
#'
#' Thibaut Jombart, Tim Taylor
#'
# -------------------------------------------------------------------------
#' @examples
#' \dontshow{.old <- data.table::setDTthreads(2)}
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#'     data(fluH7N9_china_2013, package = "outbreaks")
#'     i <- incidence(
#'         fluH7N9_china_2013,
#'         date_index = "date_of_onset",
#'         groups = "gender"
#'    )
#'    bootstrap_incidence(i)
#' }
#' \dontshow{data.table::setDTthreads(.old)}
#'
# -------------------------------------------------------------------------
#' @export
bootstrap_incidence <- function(x, randomise_groups = FALSE) {

    if (!inherits(x, "incidence2"))
        .stopf("`%s` is not an 'incidence2' object.", deparse(substitute(x)))

    assert_bool(randomise_groups)

    # prepare to restore attributes later
    row.names(x) <- NULL
    old <- attributes(x)

    # get relevant column names
    date_var <- get_date_index_name(x)
    group_vars <- get_group_names(x)
    count_var <- get_count_variable_name(x)
    count_value <- get_count_value_name(x)

    # convert to data.table
    out <- as.data.table(x)

    # overwrite the count column with the bootstrapped values
    out[, (count_value) := stats::rmultinom(1, sum(.SD[[count_value]]), .SD[[count_value]]), by = count_var]

    # randomise groups if desired
    if (randomise_groups && length(group_vars))
        out[, (group_vars) := lapply(.SD, .subset, sample.int(.N)), .SDcols = group_vars, by = count_var]

    # convert back to data frame
    setDF(out)
    old[["names"]] <- names(out)

    # restore attributes and return
    attributes(out) <- old
    out
}
