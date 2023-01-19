# Due to tidyverse/ggplot2#4786 it is safer to temporarily add a fake tbl class
# to an incidence object when printing rather than use it as part of the objects
# explicit class. Although rare for a user not to have dplyr installed, if
# ggplot2 is installed, it is not impossible. The dummy xincidence class is used
# to make this work.

#' @export
tbl_sum.xincidence <- function(x, ...) {
    header <- sprintf(
        "%s x %s",
        formatC(nrow(x), big.mark = ","),
        formatC(ncol(x), big.mark = ",")
    )
    c(incidence = header)
}

#' Print an incidence object.
#'
#' Printing of `<incidence>` objects is handled via the \pkg{pillar} package.
#'
#' @param
#'
#' x An `<incidence>` object.
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
print.incidence <- function(x, ...) {
    class(x) <- c("xincidence" ,"tbl", "data.frame")
    print(x, ...)
}
