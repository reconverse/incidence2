#' @importFrom dplyr summarise
#' @export
dplyr::summarise


#' Summarise each grouping down to one row
#'
# -------------------------------------------------------------------------
#' Method for [dplyr::summarise] that implicitly accounts for the inherent
#' grouping structure of [incidence2][incidence2::incidence] objects.
#'
# -------------------------------------------------------------------------
#' @inheritParams dplyr::mutate
#'
#' @param .data An [incidence2][incidence2::incidence] object.
#'
#' @param .by Not used as grouping structure implicit.
#'
#' @param .groups Not used.
#'
# -------------------------------------------------------------------------
#' @return
#' A [tibble][tibble::tibble].
#'
# -------------------------------------------------------------------------
#' @examples
#' \dontshow{.old <- data.table::setDTthreads(2)}
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#'     data(ebola_sim_clean, package = "outbreaks")
#'     ebola_sim_clean$linelist |>
#'         subset(!is.na(hospital)) |>
#'         incidence_(date_of_onset, hospital, interval = "isoweek") |>
#'         summarise(model = list(glm(count ~ date_index, family = "poisson")))
#' }
#' \dontshow{data.table::setDTthreads(.old)}
#'
# -------------------------------------------------------------------------
#' @seealso [dplyr::summarise] for the underlying grouping.
#'
# -------------------------------------------------------------------------
#' @export
summarise.incidence2 <- function(
    .data,
    ...,
    .by,
    .groups
) {
    if (!missing(.by))
        stop("`.by` argument cannot be used in `summary.incidence2()` as the groupings are implicit.")

    if(!missing(.groups))
        stop("`.groups` argument cannot be used with `summary.incidence2()`.")

    groupings <- c(get_count_variable_name(.data), get_group_names(.data))
    .data <- tibble::as_tibble(.data)
    summarise(.data, ..., .by = tidyr::all_of(groupings))
}
