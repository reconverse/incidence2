#' Regroup 'incidence' objects
#'
#' This function regroups an [incidence()] object across the specified groups.
#' The resulting [incidence()] object will contains counts summed over
#' the groups present in the input.
#'
#' @param x An [incidence()] object.
#' @param groups The groups to sum over.  If `NULL` (default) then the function
#'   ignores all groups.
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
#'     regroup(i)
#'
#'     regroup(i, hospital)
#'   })
#' }
#'
#' @export
regroup <- function(x, groups = NULL){

  # due to NSE notes in R CMD check
  ..count_var <- . <- NULL

  if (!inherits(x, "incidence")) abort("x should be an 'incidence' object.")

  # check groups present
  groups <- rlang::enquo(groups)
  idx <- tidyselect::eval_select(groups, x)
  groups <- names(x)[idx]
  if (length(groups) == 0) groups <- NULL

  date_var <- get_dates_name(x)
  count_var <- get_count_names(x)

  dt <- !any(vapply(x, typeof, character(1)) == "list")
  if (dt) {
    tbl <- as.data.table(x)
    tbl <- tbl[, lapply(.SD, sum, na.rm = TRUE), keyby = c(date_var, groups), .SDcols = count_var]
    setDF(tbl)
  } else {
    tbl <- grouped_df(x, c(date_var, groups))
    tbl <- summarise(tbl, across(all_of(count_var), ~sum(., na.rm = TRUE)), .groups = "drop")
  }

  tbl <- new_incidence(tbl, date = date_var, groups = groups, counts = count_var)
  if (inherits(x, "incidence2")) {
    attr(tbl, "interval") <- attr(x, "interval")
    attr(tbl, "cumulative") <- attr(x, "cumulative")
    class(tbl) <- c("incidence2", class(tbl))
  }

  tbl
}
