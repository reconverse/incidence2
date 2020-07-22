#' @importFrom generics tidy
#' @export
generics::tidy

#' @export
tidy.estimate_peak <- function(x, ...) {
  nms <- names(x)
  if (is.null(nms)) nms <- "all_groups"

  result <- lapply(
    seq_along(nms),
    function(i) {

      count_var <- get_counts_name(x[[i]]$observed)
      tibble::tibble(
        grouping = nms[i],
        estimate = x[[i]]$estimated,
        n = sum(x[[i]]$peaks[[count_var]]),
        conf.low = x[[i]]$ci[1],
        conf.high = x[[i]]$ci[2])
    }
  )

  dplyr::bind_rows(result)

}
