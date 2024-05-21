#' @export
split.incidence2 <- function(x, f, drop, ...) {

    if (!missing(f))
        stop("`f` argument cannot be used in `split.incidence2()` as the groupings are implicit.")
    if (!missing(drop))
        stop("`drop` argument cannot be used with `split.incidence2()`.")

    groupings <- c(get_count_variable_name(x), get_group_names(x))

    # note split.data.table seems to fail so we use vctrs::vec_split
    x <- tibble::as_tibble(x)
    vctrs::vec_split(x, x[groupings])[["val"]]
}
