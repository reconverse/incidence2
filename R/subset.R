# The following functions are needed to make data frame subclasses work nicely
# with dplyr.  It is based on the
# (guide)[(https://github.com/DavisVaughan/2020-06-01_dplyr-vctrs-compat)]
# by Davis Vaughan. The idea is to think to an object in terms of its invariants
# (structural information that must be true for an object to be of the specified
# subclass). Where an operation breaks these invariants, a data frame is
# returned instead of the input class.

# -------------------------------------------------------------------------
#' @export
`[.incidence2` <- function(x, i, j, ...) {
    out <- NextMethod()
    .incidence_reconstruct(out, x)
}


# -------------------------------------------------------------------------
#' @export
`[<-.incidence2` <- function(x, i, j, ..., value) {
    out <- NextMethod()
    .incidence_reconstruct(out, x)
}

# -------------------------------------------------------------------------
#' @export
`names<-.incidence2` <- function(x, value) {

    # correct essential names
    current_names <- names(x)
    vars <- c("date_index", "count_variable", "count_value", "groups")

    for (v in vars) {
        var <- attr(x, v)
        var_index <- match(var, current_names)
        attr(x, v) <- value[var_index]
    }

    out <- NextMethod()
    .incidence_reconstruct(out, x)
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

# To quote "This function is a data frame specific helper.  Currently we are
# recommended to copy in to our own package but it may eventually find it's way
# in to one of the tidy packages."
.df_reconstruct <- function(x, to) {
    attrs <- attributes(to)
    attrs$names <- names(x) # Keep column names of `x`
    attrs$row.names <- .row_names_info(x, type = 0L)
    attributes(x) <- attrs # Otherwise copy over attributes of `to`
    x
}

# -------------------------------------------------------------------------
#' @param x data.frame to have it's invariants checked
#' @param to `incidence` object we want
#'
#' @return TRUE or FALSE
#'
#' @noRd
.incidence_can_reconstruct <- function(x, to) {

    # check required variables are present
    x_names <- names(x)
    vars <- c("date_index", "count_variable", "count_value", "groups")
    for (v in vars) {
        var <- attr(to, v)
        if (!all(var %in% x_names))
            return(FALSE)
    }

    # ensure no rows are duplicated within x
    if (anyDuplicated(as.data.table(x)))
        return(FALSE)

    # else we can reconstruct
    TRUE
}

# -------------------------------------------------------------------------
#' Function to reconstruct object of incidence class
#'
#' Once we have encoded the invariant logic into .incidence_can_reconstruct, we
#' need a second function that applies that check and either performs the actual
#' reconstruction, or falls back to a bare data frame (a data frame with only
#' essential attributes).
#'
#' @param x x data.frame to have it's invariants checked
#' @param to object we want
#'
#' @noRd
.incidence_reconstruct <- function(x, to) {
    if (.incidence_can_reconstruct(x, to)) {
        .df_reconstruct(x, to)
    } else {
        # strip most attributes from data.frame
        a <- list(names = names(x), row.names = attr(x, "row.names"), class = "data.frame")
        attributes(x) <- a
        x
    }
}

# -------------------------------------------------------------------------

# Registered in `.onLoad()` in zzz.R
dplyr_reconstruct.incidence2 <- function(data, template) {
    .incidence_reconstruct(data, template)
}
