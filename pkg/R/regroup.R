#' Regroup 'incidence' objects
#'
#' This function regroups an [incidence2][incidence2::incidence] object across
#' the specified groups. The resulting [incidence2][incidence2::incidence]
#' object will contains counts aggregated over the specified groups. The only
#' difference between `regroup()` and `regroup_()` is that the latter is built
#' on top of [tidy-select][dplyr::dplyr_tidy_select] semantics for the `group`
#' input.
#'
# -------------------------------------------------------------------------
#' @param x `<incidence2>` object.
#'
#' @param groups
#'
#' `character` for `regroup()` or [tidy-select][dplyr::dplyr_tidy_select] for `regroup_()`.
#'
#' The groups to sum over.
#'
#' If `NULL` (default) then the function returns the corresponding object with
#' no groupings.
#'
# -------------------------------------------------------------------------
#' @examples
#' \dontshow{.old <- data.table::setDTthreads(2)}
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#'     data(ebola_sim_clean, package = "outbreaks")
#'     dat <- ebola_sim_clean$linelist
#'     i <- incidence(
#'         dat,
#'         date_index = "date_of_onset",
#'         groups = c("gender", "hospital")
#'     )
#'     regroup(i)
#'     regroup_(i)
#'     regroup(i, "hospital")
#'     regroup_(i, hospital)
#' }
#' \dontshow{data.table::setDTthreads(.old)}
#'
# -------------------------------------------------------------------------
#' @export
regroup <- function(x, groups = NULL) {
    if (!inherits(x, "incidence2"))
        .stop("`x` must be an <incidence2> object.")

    # group checks
    group_variables <- attr(x, "groups")
    if (is.null(groups)) {
        if (!length(group_variables))
            return(x)
    } else if (!(is.character(groups) && length(groups) >= 1L)) {
        .stop("`groups` must be NULL or a character vector.")
    } else if (!all(groups %in% group_variables)) {
        .stop("Not all variables from `groups` are groupings of `x`.")
    }

    # rebuild incidence
    date_variable <- attr(x, "date_index")
    count_variable <- attr(x, "count_variable")
    count_value <- attr(x, "count_value")

    # choose name not in use
    count_names_to <- "temp_name"
    while (count_names_to %in% names(x))
        count_names_to <- basename(tempfile())

    out <- incidence(
        x,
        date_index = date_variable,
        groups = c(groups, count_variable),
        counts = count_value,
        count_values_to = count_value,
        count_names_to = count_names_to
    )
    out[[count_names_to]] <- NULL
    attr(out, "count_variable") <- count_variable
    attr(out, "groups") <- if (is.null(groups)) character() else groups
    out
}


# -------------------------------------------------------------------------
#' @rdname regroup
#' @export
regroup_ <- function(x, groups = NULL) {
    if (!inherits(x, "incidence2"))
        stop("`x` must be an <incidence2> object.")

    groups_expr <- rlang::enquo(groups)
    groups_position <- tidyselect::eval_select(groups_expr, data = x)
    groups <- if(length(groups_position)) names(x)[groups_position] else NULL

    group_variables <- attr(x, "groups")
    if (is.null(groups) && !length(group_variables))
        return(x)

    if (!all(groups %in% group_variables))
        stop("Not all variables from `groups` are groupings of `x`.")

    # rebuild incidence
    date_variable <- attr(x, "date_index")
    count_variable <- attr(x, "count_variable")
    count_value <- attr(x, "count_value")

    # choose name not in use
    count_names_to <- "temp_name"
    while (count_names_to %in% names(x))
        count_names_to <- basename(tempfile())

    out <- incidence(
        x,
        date_index = date_variable,
        groups = c(groups, count_variable),
        counts = count_value,
        count_values_to = count_value,
        count_names_to = count_names_to
    )
    out[[count_names_to]] <- NULL
    attr(out, "count_variable") <- count_variable
    attr(out, "groups") <- if (is.null(groups)) character() else groups
    out
}
