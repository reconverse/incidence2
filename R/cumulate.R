#' Compute cumulative 'incidence'
#'
#' `cumulate()` computes the cumulative incidence over time for an `<incidence>`
#' object.
#'
#' @param x An incidence object.
#'
#' @examples
#'
#' dat <- data.frame(
#'   dates = as.integer(c(0,1,2,2,3,5,7)),
#'   groups = factor(c(1, 2, 3, 3, 3, 3, 1))
#' )
#'
#' i <- incidence(dat, date_index = "dates", groups = "groups")
#' cumulate(i)
#'
#' @export
cumulate <- function(x) {

    if (!inherits(x, "incidence"))
        stopf("`%s` is not an 'incidence' object", deparse(substitute(x)))

    group_vars <- get_group_names.incidence(x)
    count_var <- get_count_variable_name.incidence(x)
    count_value <- get_count_value_name.incidence(x)

    # can we use data.table (cannot for vctrs_rcrd objects)
    use_dt <- !any(vapply(x, typeof, character(1)) == "list")
    use_dt <- FALSE

    if (isTRUE(use_dt)) {
        out <- as.data.table(x)
        out[, (count_value) := lapply(.SD, cumsum), keyby = c(group_vars, count_var), .SDcols = count_value]
        setDF(out)
    } else {
        out <- grouped_df(x, c(group_vars, count_var))
        out <- ungroup(mutate(out, across(all_of(count_value), cumsum)))
    }
    attributes(out) <- attributes(x)
    names(out)[names(out) == count_value] <- sprintf("cumulative_%s", count_value)
    out
}
