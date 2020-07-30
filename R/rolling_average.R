#' Add and remove a rolling average
#'
#' @param x An [incidence()] object.
#' @param before how many prior dates to group with.
#'
#' @note If groups are present the average will be calculated across each
#' grouping, therefore care is required when plotting.
#'
#' @return
#'   - `rolling_average` An [incidence()] object with an additional, averaged,
#'     column.
#'   - `remove_rolling` An [incidence()] object.
#'
#' @examples
#' data(ebola_sim_clean, package = "outbreaks")
#' dat <- ebola_sim_clean$linelist
#'
#' inci <- incidence(dat,
#'                   date_index = date_of_onset,
#'                   interval = "week",
#'                   last_date = "2014-10-05",
#'                   groups = gender)
#'
#' inci %>%
#'   regroup() %>%
#'   rolling_average(before = 2) %>%
#'   plot(color = "white")
#'
#' inci %>%
#'   rolling_average(before = 2) %>%
#'   facet_plot(color = "white")
#'
#' inci %>%
#'   rolling_average() %>%
#'   remove_rolling()
#'
#' @rdname rolling_average
#' @export
rolling_average <- function(x, before = 2) {
  group_vars <- get_group_names(x)
  count_var <- get_counts_name(x)

  if (!is.null(group_vars)) {
    out <- dplyr::group_by(x, dplyr::across( {{group_vars}} ))
  } else {
    out <- x
  }

  out <- dplyr::mutate(
    out,
    rolling_average = slider::slide_dbl(
      .data[[count_var]],
      mean,
      .before = before,
      .complete = TRUE))
  attributes(out) <- attributes(x)

  # TODO - change this hackiness
  colnames(out)[length(out)] <- "rolling_average"
  attr(out, "rolling_average") <- "rolling_average"
  attr(out, "before") <- before
  out
}


#' @rdname rolling_average
#' @export
remove_rolling <- function(x) {
  ra <- attr(x, "rolling_average")
  x[[ra]] <- NULL
  attr(x, "rolling_average") <- NULL
  attr(x, "before") <- NULL
  x
}



