#' Access various elements of an incidence object
#'
#' @param x An [incidence()] object.
#' @param ... Not used.
#'
#' @examples
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#'   withAutoprint({
#'     data(ebola_sim_clean, package = "outbreaks")
#'     dat <- ebola_sim_clean$linelist
#'     i <- incidence(dat,
#'                    date_index = date_of_onset,
#'                    groups = c(gender, hospital))
#'
#'     get_count_vars(i)
#'
#'     get_group_vars(i)
#'
#'     get_date_vars(i)
#'
#'     get_date_group_vars(i)
#'
#'     get_interval(i)
#'
#'     get_n(i)
#'
#'     get_timespan(i)
#'   })
#' }
#'
#' @name accessors
NULL
