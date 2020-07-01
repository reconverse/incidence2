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
