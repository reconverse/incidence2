#' Functions now defunct in package incidence2
#'
#' These functions are now defunct.
#'
#' @name incidence2-defunct
#' @keywords internal
NULL



# -------------------------------------------------------------------------
#' @rdname incidence2-defunct
#' @export
new_incidence <- function(
    x,
    date,
    groups = NULL,
    counts,
    measurements = NULL,
    validate = TRUE
) {
    .Defunct(
        msg = "`new_incidence` is defunct and has been removed from incidence2."
    )
}

# -------------------------------------------------------------------------
#' @rdname incidence2-defunct
#' @export
validate_incidence <- function(x) {
    .Defunct(
        msg = "`validate_incidence` is defunct and has been removed from incidence2."
    )
}

# -------------------------------------------------------------------------
#' @rdname incidence2-defunct
#' @export
build_incidence <- function(
    x,
    date_index,
    groups = NULL,
    counts = NULL,
    na_as_group = TRUE,
    FUN = identity,
    args = list()
) {
    .Defunct(
        msg = "`build_incidence` is defunct and has been removed from incidence2."
    )
}

# -------------------------------------------------------------------------
#' @rdname incidence2-defunct
get_n <- function(x) {
    .Defunct(
        msg = "`get_n` is defunct and has been removed from incidence2."
    )
}

# -------------------------------------------------------------------------
#' @rdname incidence2-defunct
#' @export
get_interval <- function(x, ...) {
    .Defunct(
        msg = "`get_interval` is defunct and has been removed from incidence2."
    )
}

# -------------------------------------------------------------------------
#' @rdname incidence2-defunct
#' @export
get_timespan <- function(x, ...) {
    .Defunct(
        msg = "`get_timespan` is defunct and has been removed from incidence2."
    )
}

# -------------------------------------------------------------------------
#' @rdname incidence2-defunct
#' @export
facet_plot <- function(x, ...) {
    .Defunct(
        msg = "`facet_plot` is defunct and has been removed from incidence2."
    )
}
