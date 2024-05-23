#' @importFrom tidyr nest
#' @export
tidyr::nest


#' Nest rows into a list-column of data frames
#'
# -------------------------------------------------------------------------
#' Method for [tidyr::nest] that implicitly accounts for the inherent
#' grouping structure of incidence2 objects.
#'
# -------------------------------------------------------------------------
#' @inheritParams tidyr::nest
#'
#' @param .data An [incidence2][incidence2::incidence] object.
#'
#' @param ... Not used.
#'
#' @param .by Not used.
#'
#' @param .names_sep Not used.
#'
# -------------------------------------------------------------------------
#' @return
#' A nested [tibble][tibble::tibble] with rows corresponding to the count
#' variable and (optionally) group columns of the input object.
#'
# -------------------------------------------------------------------------
#' @examples
#' \dontshow{.old <- data.table::setDTthreads(2)}
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#'     data(ebola_sim_clean, package = "outbreaks")
#'     ebola_sim_clean$linelist |>
#'         subset(!is.na(hospital)) |>
#'         incidence_(date_of_onset, hospital, interval = "isoweek") |>
#'         nest()
#' }
#' \dontshow{data.table::setDTthreads(.old)}
#'
# -------------------------------------------------------------------------
#' @seealso [tidyr::nest] for the underlying generic.
#'
# -------------------------------------------------------------------------
#' @export
nest.incidence2 <- function(.data, ..., .by, .key, .names_sep) {
    if(!missing(.by))
        stop("`.by` argument cannot be used in `nest.incidence2()` as the groupings are implicit.")

    if(!missing(.names_sep))
        stop("`.names_sep` argument cannot be used with `nest.incidence2()`.")

    groupings <- c(get_count_variable_name(.data), get_group_names(.data))
    nest(as.data.frame(.data), .by = tidyr::all_of(groupings), .key = .key)
}
