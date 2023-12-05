#' Regroup 'incidence' objects
#'
#' This function regroups an `<incidence2>` object across the specified groups.
#' The resulting `<incidence2>` object will contains counts summed over the
#' groups present in the input.
#'
# -------------------------------------------------------------------------
#' @param x `<incidence2>` object.
#'
#' @param groups `[character]`
#'
#' The groups to sum over.
#'
#' If `NULL` (default) then the function returns the corresponding object with
#' no groupings.
#'
# -------------------------------------------------------------------------
#' @examples
#' \dontshow{data.table::setDTthreads(2)}
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#' \dontshow{withAutoprint(\{}
#'     data(ebola_sim_clean, package = "outbreaks")
#'     dat <- ebola_sim_clean$linelist
#'     i <- incidence(
#'         dat,
#'         date_index = "date_of_onset",
#'         groups = c("gender", "hospital")
#'     )
#'     regroup(i)
#'     regroup(i, "hospital")
#' \dontshow{\})}
#' }
#'
# -------------------------------------------------------------------------
#' @export
regroup <- function(x, groups = NULL){

    if (!inherits(x, "incidence2"))
        stopf("`x` must be an <incidence2> object.")

    # group checks
    group_variables <- attr(x, "groups")
    if (is.null(groups)) {
        if (!length(group_variables))
            return(x)
    } else if(!(is.character(groups) && length(groups) >= 1L)) {
        stopf("`groups` must be NULL or a character vector.")
    } else if (!all(groups %in% group_variables)) {
        stopf("Not all variables from `groups` are groupings of `x`.")
    }

    # rebuild incidence
    date_variable <- attr(x, "date_index")
    count_variable <- attr(x, "count_variable")
    count_value <- attr(x, "count_value")

    # this is a little hacky
    count_names_to = "temp_name"
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

