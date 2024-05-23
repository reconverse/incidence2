#' @importFrom dplyr mutate
#' @export
dplyr::mutate


#' Create, modify, and delete incidence2 columns
#'
# -------------------------------------------------------------------------
#' Method for [dplyr::mutate] that implicitly accounts for the inherent
#' grouping structure of incidence2 objects.
#'
# -------------------------------------------------------------------------
#' @inheritParams dplyr::mutate
#'
#' @param .data An [incidence2][incidence2::incidence] object.
#'
#' @param .by Not used as grouping structure implicit.
#'
# -------------------------------------------------------------------------
#' @return
#' A modified [incidence2][incidence2::incidence] object if the necessary
#' invariants are preserved, otherwise a [tibble][tibble::tibble].
#'
# -------------------------------------------------------------------------
#' @examples
#' \dontshow{.old <- data.table::setDTthreads(2)}
#' if (requireNamespace("outbreaks", quietly = TRUE) && requireNamespace("ggplot2", quietly = TRUE)) {
#'     data(ebola_sim_clean, package = "outbreaks")
#'     ebola_sim_clean$linelist |>
#'         subset(!is.na(hospital)) |>
#'         incidence_(date_of_onset, hospital, interval = "isoweek") |>
#'         mutate(ave = data.table::frollmean(count, n = 3L, align = "right")) |>
#'         plot(border_colour = "white", angle = 45) +
#'         ggplot2::geom_line(ggplot2::aes(x = date_index, y = ave))
#' }
#' \dontshow{data.table::setDTthreads(.old)}
#'
# -------------------------------------------------------------------------
#' @seealso [dplyr::mutate] for the underlying generic.
#'
# -------------------------------------------------------------------------
#' @export
mutate.incidence2 <- function(
    .data,
    ...,
    .by,
    .keep = c("all", "used", "unused", "none"),
    .before = NULL,
    .after = NULL
) {
    if(!missing(.by)) {
        stop("`.by` argument is not used in `mutate.incidence2()` as the groupings are implicit.")
    }

    groupings <- c(get_count_variable_name(.data), get_group_names(.data))
    out <- tibble::as_tibble(.data)
    out <- mutate(
        out,
        ...,
        .by = tidyr::all_of(groupings),
        .keep = .keep,
        .before = {{.before}},
        .after = {{.after}}
    )
    .incidence_reconstruct(out, .data)
}
