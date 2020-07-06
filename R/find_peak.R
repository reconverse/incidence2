#' Find the peak date of an incidence curve
#'
#' This function can be used to find the peak of an epidemic curve stored as an
#' `incidence` object.
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}, Zhian N. Kamvar
#'   \email{zkamvar@@gmail.com}
#'
#' @md
#'
#' @param x An `incidence` object.
#' @param regroup If `TRUE` (default), any groups will be regrouped before finding
#'   a peak. If `FALSE`, separate peaks will be found for each group.
#'
#' @return The date of the (first) highest incidence in the data.
#'
#' @seealso [estimate_peak()] for bootstrap estimates of the peak time
#'
#' @examples
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#'   withAutoprint( {
#'     # load data and create incidence
#'     data(fluH7N9_china_2013, package = "outbreaks")
#'     i <- incidence(fluH7N9_china_2013, date_index = date_of_onset)
#'     i
#'
#'     # one simple bootstrap
#'     x <- bootstrap(i)
#'     x
#'
#'     ## find 95% CI for peak time using bootstrap
#'     find_peak(i)
#'   })
#' }
#' @export
find_peak <- function(x, regroup = TRUE) {
  if (!inherits(x, "incidence")) {
    stop(sprintf("`%s` is not an incidence object", deparse(substitute(x))))
  }

  count_var <- get_count_vars(x)
  group_vars <- get_group_vars(x)
  date_var <- get_date_vars(x)[1]

  if ((length(group_vars) > 0) && regroup) {
    msg <- paste("`%s` is stratified by groups",
                 "regrouping groups before finding peaks",
                 sep = "\n")
    message(sprintf(msg, deparse(substitute(x))))
    x <- regroup(x)

  } else if (length(group_vars) > 0) {
    x <- group_by(x, across(all_of(group_vars)))
  }

  dplyr::slice_max(x, .data[[count_var]], order_by = .data[[date_var]], n = 1)
}
