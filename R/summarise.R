#' @importFrom dplyr summarise
#' @export
summarise.incidence2 <- function(
    .data,
    ...,
    .by,
    .groups
) {
    if(!missing(.by))
        stop("`.by` argument cannot be used in `summary.incidence2()` as the groupings are implicit.")

    if(!missing(.groups))
        stop("`.groups` argument cannot be not used with `summary.incidence2()`.")

    groupings <- c(get_count_variable_name(.data), get_group_names(.data))
    .data <- tibble::as_tibble(.data)
    summarise(.data, ..., .by = tidyr::all_of(groupings))
}
