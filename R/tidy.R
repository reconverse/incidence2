# See https://github.com/tidyverse/dplyr/issues/6633#issuecomment-1372383098
# for why we cannot use NextMethod with tidyselect arguments :(

#' @importFrom dplyr mutate
#' @export
mutate.incidence2 <- function(
    .data,
    ...,
    .by,
    .keep = c("all", "used", "unused", "none"),
    .before = NULL,
    .after = NULL
) {
    if(!missing(.by))
        stop("`.by` argument is not used in `mutate.incidence2()` as the groupings are implicit.")

    groupings <- c(get_count_variable_name(.data), get_group_names(.data))
    out <- tibble::as_tibble(.data)
    out <- mutate(out, ..., .by = tidyr::all_of(groupings), .keep = .keep, .before = {{.before}}, .after = {{.after}})
    .incidence_reconstruct(out, .data)
}

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

#' @importFrom dplyr reframe
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
