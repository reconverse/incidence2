#' @importFrom pillar tbl_sum
#' @export
tbl_sum.incidence2 <- function(x, ...) {

    # class and dimensions
    header <- sprintf(
        "%s x %s",
        formatC(nrow(x), big.mark = ","),
        formatC(ncol(x), big.mark = ",")
    )

    # counts
    counts <- unique(get_count_variable.incidence2(x))
    counts <- toString(counts)

    # output
    out <- c(incidence = header, `count vars` = toString(counts))

    # add groups if present
    groups <- get_group_names.incidence2(x)
    if (length(groups)) {
        groups <- toString(groups)
        out <- c(out, groups = groups)
    }

    out
}
