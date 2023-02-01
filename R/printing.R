# Due to tidyverse/ggplot2#4786 it is safer to temporarily add a fake tbl class
# to an incidence object when printing rather than use it as part of the objects
# explicit class. Although rare for a user not to have dplyr installed, if
# ggplot2 is installed, it is not impossible. The dummy xincidence class is used
# to make this work.

#' @export
tbl_sum.xincidence2 <- function(x, ...) {

    # class and dimensions
    header <- sprintf(
        "%s x %s",
        formatC(nrow(x), big.mark = ","),
        formatC(ncol(x), big.mark = ",")
    )

    # counts
    counts <- unique(get_count_variable.incidence2(x)[[1L]])
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

#' Print an incidence object.
#'
#' Printing of `<incidence2>` objects is handled via the \pkg{pillar} package.
#'
#' @param
#'
#' x An `<incidence2>` object.
#'
#' @param ...
#'
#' Additional arguments passed through to `pillar::tbl_format_setup()`.
#'
#' @examples
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#' \dontshow{withAutoprint(\{}
#'     data(ebola_sim_clean, package = "outbreaks")
#'     dat <- ebola_sim_clean$linelist
#'
#'     (out <- incidence(dat, "date_of_onset"))
#'
#'     # use `n` to print more lines
#'     print(out, n = 20L)
#' \dontshow{\})}
#' }
#'
#' @export
print.incidence2 <- function(x, ...) {
    class(x) <- c("xincidence2" ,"tbl", "data.frame")
    print(x, ...)
}
