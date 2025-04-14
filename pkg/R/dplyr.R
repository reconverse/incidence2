#' @importFrom dplyr mutate
#' @export
dplyr::mutate

#' @importFrom tidyr nest
#' @export
tidyr::nest

#' @importFrom dplyr select
#' @export
dplyr::select

#' @importFrom dplyr summarise
#' @export
dplyr::summarise

#' @importFrom tidyr unpack
#' @export
tidyr::unpack


#' @importFrom tidyr unnest
#' @export
tidyr::unnest


#' dplyr and tidyr verbs
#'
# -------------------------------------------------------------------------
#' [dplyr][dplyr::dplyr] and [tidyr][tidyr::tidyr] methods that implicitly
#' account for the inherent grouping structure of incidence2 objects.
#'
# -------------------------------------------------------------------------
#' @param .data An [incidence2][incidence2::incidence] object.
#'
#' @param .by Not used as grouping structure implicit.
#'
#' @param .names_sep Not used.
#'
#' @param .groups Not used.
#'
#' @param ... Only used by `mutate()` and `summarise()` and, in which case,
#' passed to underlying dplyr function.
#'
#' @inheritParams dplyr::mutate
#'
#' @inheritParams tidyr::nest
#'
# -------------------------------------------------------------------------
#' @return
#' - For `mutate()` a modified [incidence2][incidence2::incidence] object if the
#'   necessary invariants are preserved, otherwise a [tibble][tibble::tibble].
#'
#' - For `nest()` a nested [tibble][tibble::tibble] with rows corresponding to
#'   the count variable and (optionally) group columns of the input object.
#'
#' - For summarise a [tibble][tibble::tibble] with rows corresponding to the
#'   underlying groupings. The columns are a combination of the grouping keys
#'   and the summary expressions provided.
#'
# -------------------------------------------------------------------------
#' @examples
#' \dontshow{.old <- data.table::setDTthreads(2)}
#' if (requireNamespace("outbreaks", quietly = TRUE) && requireNamespace("ggplot2", quietly = TRUE)) {
#'     data(ebola_sim_clean, package = "outbreaks")
#'     x <- subset(ebola_sim_clean$linelist, !is.na(hospital))
#'     dat <- incidence_(x, date_of_onset, hospital, interval = "isoweek")
#'
#'     mutate(dat, ave = data.table::frollmean(count, n = 3L, align = "right")) |>
#'         plot(border_colour = "white", angle = 45) +
#'         ggplot2::geom_line(ggplot2::aes(x = date_index, y = ave))
#'
#'     nest(dat)
#'
#'     summarise(dat, model = list(glm(count ~ date_index, family = "poisson")))
#' }
#' \dontshow{data.table::setDTthreads(.old)}
#'
# -------------------------------------------------------------------------
#' @seealso
#' [dplyr::mutate], [tidyr::nest] and [dplyr::summarise] for the underlying
#' generics.
#'
# -------------------------------------------------------------------------
#' @name dplyr-verbs
NULL

# -------------------------------------------------------------------------
#' @rdname dplyr-verbs
#' @export
mutate.incidence2 <- function(
        .data,
        ...,
        .by,
        .keep = c("all", "used", "unused", "none"),
        .before = NULL,
        .after = NULL
) {
    if (!missing(.by)) {
        .stop("`.by` argument is not used in `mutate.incidence2()` as the groupings are implicit.")
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


# -------------------------------------------------------------------------
#' @rdname dplyr-verbs
#' @export
nest.incidence2 <- function(.data, ..., .by, .key, .names_sep) {
    if (!missing(.by))
        .stop("`.by` argument cannot be used in `nest.incidence2()` as the groupings are implicit.")

    if (!missing(.names_sep))
        .stop("`.names_sep` argument cannot be used with `nest.incidence2()`.")

    groupings <- c(get_count_variable_name(.data), get_group_names(.data))
    nest(as.data.frame(.data), .by = tidyr::all_of(groupings), .key = .key)
}


# -------------------------------------------------------------------------
#' @rdname dplyr-verbs
#' @export
summarise.incidence2 <- function(
    .data,
    ...,
    .by,
    .groups
) {
    if (!missing(.by))
        .stop("`.by` argument cannot be used in `summary.incidence2()` as the groupings are implicit.")

    if(!missing(.groups))
        .stop("`.groups` argument cannot be used with `summary.incidence2()`.")

    groupings <- c(get_count_variable_name(.data), get_group_names(.data))
    .data <- tibble::as_tibble(.data)
    summarise(.data, ..., .by = tidyr::all_of(groupings))
}
