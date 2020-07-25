#' @export
rolling_average <- function(dat, before = 2, after = 0) {
  group_vars <- get_group_names(dat)
  count_var <- get_counts_name(dat)

  if (!is.null(group_vars)) {
    out <- dplyr::group_by(dat, dplyr::across( {{group_vars}} ))
  } else {
    out <- dat
  }

  out <- dplyr::mutate(
    out,
    rolling_average = slider::slide_dbl(
      .data[[count_var]],
      mean,
      .before = before,
      .after = after,
      .complete = TRUE))
  attributes(out) <- attributes(dat)

  # TODO - change this hackiness
  colnames(out)[length(out)] <- "rolling_average"
  attr(out, "rolling_average") <- before
  out
}



