# -------------------------------------------------------------------------
#' Convert incident object to dataframe
#'
#' @param x An [incidence()] object.
#'
#' @param ... Not used.
#'
#' @examples
#' dat <- data.frame(dates = Sys.Date() + 1:100,
#'                   names = rep(c("Jo", "John"), 5))
#'
#' dat <- incidence(dat, date_index = dates, groups = names)
#' as.data.frame(dat)
#'
#' @export
as.data.frame.incidence <- function(x, ...) {
  ellipsis::check_dots_empty()
  vctrs::new_data_frame(x)
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
#' Convert incident object to a tibble
#'
#' @param x An [incidence()] object.
#'
#' @param ... Not used.
#'
#' @examples
#' dat <- data.frame(dates = Sys.Date() + 1:100,
#'                   names = rep(c("Jo", "John"), 5))
#'
#' dat <- incidence(dat, date_index = dates, groups = names)
#' as_tibble(dat)
#'
#' @export
#' @name as_tibble
as_tibble.incidence <- function(x, ...) {
  ellipsis::check_dots_empty()
  new_bare_tibble(x)
}
# -------------------------------------------------------------------------

#' @importFrom tibble as_tibble
#' @export
#' @name as_tibble
tibble::as_tibble()

# -------------------------------------------------------------------------

#' @rdname as_incidence
#' @export
as_incidence <- function(x, date_index, counts_var,
                         group_vars = NULL, interval = 1L,
                         standard = TRUE, ...) {

  UseMethod("as_incidence", x)
}


#' @rdname as_incidence
#' @aliases as_incidence.default
#' @export
as_incidence.default <- function(x, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}


#' @rdname as_incidence
#' @aliases as_incidence.default
#' @export
as_incidence.data.frame <- function(x, date_index, counts_var,
                                    group_vars = NULL, interval = 1L,
                                    standard = TRUE, ...) {

  # change date_index and group to character vectors
  dates <- arg_values(!!rlang::enexpr(date_index))
  count <- arg_values(!!rlang::enexpr(counts_var))
  groups <- arg_values(!!rlang::enexpr(group_vars))

  stopifnot(
    "The argument `date_index` should be of length one" =
      (length(dates) == 1),
    "The argument `counts_var` should be of length one" =
      (length(count) == 1),
    "The argument `standard` must be either `TRUE` or `FALSE`." =
      (is.logical(standard))
  )

  dat <- x[c(dates, groups)]
  if (anyDuplicated(dat)) {
    stop("Cannot convert a dataframe with duplicated rows into an incidence object")
  }

  dat <- dat[rep(seq_len(nrow(dat)), x[[count]]), ]
  incidence(dat,
            date_index = !!rlang::enexpr(dates),
            groups = !!rlang::enexpr(groups),
            interval = interval,
            standard = standard)

}
