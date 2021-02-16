#' Check for presence of names
#'
#' @param nms A vector of names you whose presence you want to check.
#' @param column_names Data frame names you wish to check against.
#'
#' @return Will error if any names are not present, otherwise return NULL
#'   invisibly.
#'
#' @noRd
check_presence <- function(nms, column_names) {
  if (!is.null(nms)) {
    missing <- nms[!nms %in% column_names]
    if (length(missing > 0)) {
      msg <- paste("variable", missing, "not present in dataframe",
        collapse = "\n"
      )
      stop(msg, call. = FALSE)
    }
  }
  invisible(NULL)
}
