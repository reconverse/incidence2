#' @importFrom tidyr nest
#' @export
nest.incidence2 <- function(.data, ..., .by, .key, .names_sep) {
    if(!missing(.by))
        stop("`.by` argument cannot be used in `nest.incidence2()` as the groupings are implicit.")

    if(!missing(.key))
        stop("`.key` argument cannot be used with `nest.incidence2()`.")

    if(!missing(.names_sep))
        stop("`.names_sep` argument cannot be used with `nest.incidence2()`.")

    groupings <- c(get_count_variable_name(.data), get_group_names(.data))
    nest(as.data.frame(.data), .by = tidyr::all_of(groupings))
}
