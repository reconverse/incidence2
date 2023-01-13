# (old comment)
# Due to tidyverse/ggplot2#4786 it is safer to temporarily add a fake tbl class
# to an incidence object when printing rather than use it as part of the objects
# explicit class. Although rare for a user not to have dplyr installed, if
# ggplot2 is installed, it is not impossible. The dummy xincidence class is used
# to make this work.
#
# (2022-01-13)
# I've now pulled in dplyr as a hard (import) dependency which removes the need
# for this approach. We could instead make an incidence object always be of
# class c("incidence" ,"tbl", "data.frame"). I'm still leaving this approach as
# is though because:
#    1 - it makes it easier to drop dplyr if we wish in future;
#    2 - it protects against the case where someone may serialise an incidence
#        object for later loading and plotting. In this case, even if they do
#        not load incidence, it will be treated like a normal data frame on
#        deserialisation.
#    3 - it protects against other misuse of the tbl class by external packages.

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
