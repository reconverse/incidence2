#' Compute cumulative 'incidence'
#'
#' `cumulate()` computes the cumulative incidence over time for an
#' `<incidence2>` object.
#'
#' @param x `[incidence2]` object.
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

    if (!inherits(x, "incidence2"))
        stopf("`%s` is not an 'incidence2' object", deparse(substitute(x)))

    group_vars <- get_group_names.incidence2(x)
    count_var <- get_count_variable_name.incidence2(x)
    count_value <- get_count_value_name.incidence2(x)

    out <- as.data.table(x)
    out[, (count_value) := lapply(.SD, cumsum), keyby = c(group_vars, count_var), .SDcols = count_value]
    setDF(out)

    attributes(out) <- attributes(x)
    names(out)[names(out) == count_value] <- sprintf("cumulative_%s", count_value)
    out
}
