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
    if(!missing(.by)) {
        stop("`.by` argument is not used in `mutate.incidence2()` as the groupings are implicit.")
    }

    groupings <- c(get_count_variable_name(.data), get_group_names(.data))
    out <- tibble::as_tibble(.data)
    out <- mutate(out, ..., .by = tidyr::all_of(groupings), .keep = .keep, .before = {{.before}}, .after = {{.after}})
    .incidence_reconstruct(out, .data)
}
