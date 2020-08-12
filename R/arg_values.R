#' Return quoted arguments of given expression
#'
#' @param args Expression such  `x`, `"x"`, `c(x, y)` or `c("x", "y")`.
#'
#' @return The quoted arguments.
#'
#' @examples
#' arg_values(x)
#' arg_values("x")
#' arg_values(c(x, y))
#' arg_values(c("x", "y"))
#'
#' @noRd
arg_values <- function(args) {
  args_expr <- rlang::enexpr(args)
  if (is.null(args_expr)) {
    NULL
  } else if (length(args_expr) == 1) {
    as.character(args_expr)
  } else {
    as.character(args_expr[-1])
  }
}
