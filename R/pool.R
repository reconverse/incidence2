#' Pool 'incidence' across groups
#'
#' This function pools incidence across specified groups of an `incidence`
#' object. The resulting [incidence()] object will contains counts summed over
#' the groups present in the input.
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @seealso The [incidence()] function to generate the 'incidence'
#' objects.
#'
#' @param x An [incidence()] object.
#' @param groups The groups to sum over.  If `NULL` (default) then the function
#'   ignores all groups.
#'
#' @export
pool <- function(x, groups = NULL){

  if (!inherits(x, "incidence")) {
    stop(sprintf(
      "x should be an 'incidence' object.",
      class(x)))
  }

  # check groups present
  groups <- arg_values(!!rlang::enexpr(groups))
  column_names <- names(x)
  check_presence(groups, column_names)

  date_var <- attr(x, "date")
  count_var <- attr(x, "count")
  cumulate <- attr(x, "cumulative")
  interval <- attr(x, "interval")

  tbl <- group_by(x, across(all_of(date_var)))
  if (!is.null(groups)) {
    tbl <- group_by(tbl, across(all_of(groups)), .add = TRUE)
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
                            class = "incidence"
  )
  tibble::validate_tibble(tbl)
}
