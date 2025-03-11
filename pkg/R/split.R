#' Divide an incidence2 object in to it's implicit groupings
#'
# -------------------------------------------------------------------------
#' Split divides and [incidence2][incidence2::incidence] object in to it's
#' underlying groupings (count variable and optionally groups).
#'
# -------------------------------------------------------------------------
#' @param x An [incidence2][incidence2::incidence] object.
#'
#' @param f Not used. Present only for generic compatibility.
#'
#' @param drop Not used. Present only for generic compatibility.
#'
#' @param ... Not used. Present only for generic compatibility.
#'
# -------------------------------------------------------------------------
#' @return
#' A list of tibbles contained the split data. This list also has a "key"
#' attribute which is a tibble with rows corresponding to the grouping of
#' each split.
#'
# -------------------------------------------------------------------------
#' @examples
#' \dontshow{.old <- data.table::setDTthreads(2)}
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#'     data(ebola_sim_clean, package = "outbreaks")
#'     ebola_sim_clean$linelist |>
#'         subset(!is.na(hospital)) |>
#'         incidence_(date_of_onset, hospital, interval = "isoweek") |>
#'         split()
#' }
#' \dontshow{data.table::setDTthreads(.old)}
#'
# -------------------------------------------------------------------------
#' @seealso
#' [vctrs::vec_split()] on which `split.incidence2()` is built.
#'
# -------------------------------------------------------------------------
#' @export
split.incidence2 <- function(x, f, drop, ...) {

    if (!missing(f))
        stop("`f` argument cannot be used in `split.incidence2()` as the groupings are implicit.")
    if (!missing(drop))
        stop("`drop` argument cannot be used with `split.incidence2()`.")

    groupings <- c(get_count_variable_name(x), get_group_names(x))

    # note split.data.table seems to fail so we use vctrs::vec_split
    x <- tibble::as_tibble(x)
    split <- vctrs::vec_split(x, x[groupings])
    key <- split[["key"]]
    split <- split[["val"]]
    attr(split, "key") <- key
    split
}
