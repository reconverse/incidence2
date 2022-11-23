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
        msg = sprintf(
            "`%s` is defunct and has been removed from incidence2.",
            as.character(sys.call()[1L])
        )
    )
}

# -------------------------------------------------------------------------
#' @rdname incidence2-defunct
#' @export
validate_incidence <- function(x) {
    .Defunct(
        msg = sprintf(
            "`%s` is defunct and has been removed from incidence2.",
            as.character(sys.call()[1L])
        )
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
        msg = sprintf(
            "`%s` is defunct and has been removed from incidence2.",
            as.character(sys.call()[1L])
        )
    )
}

# -------------------------------------------------------------------------
#' @rdname incidence2-defunct
#' @export
get_n <- function(x) {
    .Defunct(
        msg = sprintf(
            "`%s` is defunct and has been removed from incidence2.",
            as.character(sys.call()[1L])
        )
    )
}

# -------------------------------------------------------------------------
#' @rdname incidence2-defunct
#' @export
get_interval <- function(x, ...) {
    .Defunct(
        msg = sprintf(
            "`%s` is defunct and has been removed from incidence2.",
            as.character(sys.call()[1L])
        )
    )
}

# -------------------------------------------------------------------------
#' @rdname incidence2-defunct
#' @export
get_timespan <- function(x, ...) {
    .Defunct(
        msg = sprintf(
            "`%s` is defunct and has been removed from incidence2.",
            as.character(sys.call()[1L])
        )
    )
}

# -------------------------------------------------------------------------
#' @rdname incidence2-defunct
#' @export
facet_plot <- function(x, ...) {
    .Defunct(
        msg = sprintf(
            "`%s` is defunct and has been removed from incidence2.",
            as.character(sys.call()[1L])
        )
    )
}
