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
#'     i %>% regroup()
#'
#'     i %>% regroup(hospital)
#'   })
#' }
#'
#' @export
regroup <- function(x, groups = NULL){

  if (!inherits(x, "incidence2")) {
    stop(sprintf(
      "x should be an 'incidence2' object.",
      class(x)))
  }

  # check groups present
  groups <- rlang::enquo(groups)
  idx <- tidyselect::eval_select(groups, x)
  groups <- names(x)[idx]
  column_names <- names(x)
  check_presence(groups, column_names)

  date_var <- get_dates_name(x)
  count_var <- get_counts_name(x)
  cumulate <- attr(x, "cumulative")
  interval <- get_interval(x)

  if (!is.null(groups)) {
    tbl <- grouped_df(x, c(date_var, groups))
  } else {
    tbl <- grouped_df(x, date_var)
  }
  tbl <- summarise(tbl, count = sum(.data[[count_var]]))

  # create subclass of tibble
  tbl <- tibble::new_tibble(tbl,
                            groups = groups,
                            date = date_var,
                            count = count_var,
                            interval = interval,
                            cumulative = cumulate,
                            nrow = nrow(tbl),
                            class = "incidence2"
  )
  tibble::validate_tibble(tbl)


}





