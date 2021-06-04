#' Incidence constructor and validator
#'
#' Creates or validates an incidence object. Mainly of use to those developing
#'   packages to work with incidence objects.
#'
#' @details
#'   `new_incidence()` creates a new incidence object which is a subclass
#'    of a tibble (i.e. class `incidence`, `tbl_df`, `tbl` and `data.frame`).
#'
#'  `validate_incidence()` checks the object for internal consistency. For an
#'    object to be considered an incidence object it must:
#'      * `inherit` the incidence and data.frame class;
#'      * have a single column representing the date_index with the name of this
#'        variable being stored in the `date` attribute;
#'      * have one or more columns representing the counts with the name of
#'        these variables being stored in the `counts` attribute;
#'      * have zero or more columns representing groups with, if and only if
#'        present, the names of these being stored in the `groups` attribute;
#'      * have zero or more columns representing measurement with, if and only
#'        if present, the names of these being stored in the `measurements`
#'        attribute;
#'      * not have duplicated rows with regards to the date and group variables.
#'
#' @param x An incidence-like object
#' @param date The time index of `x.
#' @param groups An optional vector giving the names of the groups in `x`.
#' @param counts The count variables of `x`
#' @param measurements An optional vector giving the names of measurement
#'   variables in `x`.
#' @param validate A logical value indicating whether to validate the input.
#'   If FALSE, only minimal checks are made which can give a performance
#'   advantage if so desired.
#'
#' @return An incidence object (invisibly for `validate_incidence()`)
#'
#' @export
new_incidence <- function(x, date, groups = NULL, counts, measurements = NULL,
                          validate = TRUE) {

  out <- new_tibble(
    x,
    date = date,
    groups = groups,
    counts = counts,
    measurements = measurements,
    nrow = nrow(x),
    class = "incidence"
  )

  if (validate) (validate_incidence(validate_tibble(out))) else out
}

#' @rdname new_incidence
#' @export
validate_incidence <- function(x) {

  # pull out the relevant attributes
  date <- attr(x, "date")
  groups <- attr(x, "groups")
  counts <- attr(x, "counts")
  measurements <- attr(x, "measurements")

  # check types and lengths
  if (!inherits_all(x, c("incidence", "data.frame"))) {
    abort("`x` must be a data frame with subclass incidence")
  }
  vec_assert(date, ptype = character(), size = 1L)
  vec_assert(counts, ptype = character())
  stopifnot("`counts` must have length greater than 0" = length(counts) > 0)

  if(!is.null(groups)) vec_assert(groups, ptype = character())
  if(!is.null(measurements)) vec_assert(measurements, ptype = character())

  # check that all attributes are present as column headers
  columns_present(x, date)
  columns_present(x, groups)
  columns_present(x, counts)
  columns_present(x, measurements)

  # check for duplicated rows
  if(vec_duplicate_any(x[c(date, groups)])) {
    abort("An incidence object cannot have duplicated rows with regards to date and group variables")
  }

  invisible(x)
}


columns_present <- function(x, nms) {
  res <- if (length(nms)) all(nms %in% names(x)) else TRUE
  if (!res) {
    msg <- sprintf(
      "Not all variables in the `%s` attribute are present in the incidence object",
      deparse(substitute(nms))
    )
    abort(msg)
  }
}
