# Based on this stackoverflow answer (https://stackoverflow.com/a/57709169)
# from Valeri Voev

#' Return quoted arguments of given expression
#'
#' @param args Expression such  `x`, `"x"`, `c(x, y)` or `c("x", "y")`.
#'
#' @return The quoted arguments.
#'
#' @examples
#' return_args(x)
#' return_args("x")
#' return_args(c(x, y))
#' return_args(c("x", "y"))
#'
#' @noRd
return_args <- function(args) {
  args_expr <- rlang::enexpr(args)
  if(length(args_expr) == 1) {
    args_vars <- as.list(args_expr)
  } else {
    args_vars <- as.list(args_expr)[-1]
  }
  vapply(args_vars, rlang::quo_name, character(1))
}


#' Return names of arguments of given expression
#'
#' @param args Expression such  `c(a = x, b = y)` or `c(x, y)`.
#'
#' @return The arguments names if they exist.
#'
#' @examples
#' return_args(c(a = x, b = y))
#' return_args(c(a = x, y))
#' @noRd
return_args_names <- function(args) {
  expr <- rlang::enexpr(args)
  names(return_args(!!expr))
}
