#' @importFrom dplyr reframe
#' @export
dplyr::reframe

#' @export
reframe.incidence2 <- function(
    .data,
    ...,
    .by
) {
    if(!missing(.by)) {
        stop("`.by` argument cannot be used in `reframe.incidence2()` as the groupings are implicit.")
    }

    groupings <- c(get_count_variable_name(.data), get_group_names(.data))
    .data <- tibble::as_tibble(.data)
    reframe(.data, ..., .by = tidyr::all_of(groupings))
}
