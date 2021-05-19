# The following functions are needed to make `incidence()`` objects work nicely
# with dplyr.  It is based on the
# (guide)[(https://github.com/DavisVaughan/2020-06-01_dplyr-vctrs-compat)]
# by Davis Vaughan.  The idea is to think to an incidence object in terms of
# it's invariants (structural information that must be true for an object to be
# of class incidence). Where an operation breaks these invariants a tibble is
# returned instead of an incidence object.


#' Check whether incidence object invariants hold
#'
#' @param x data.frame to have it's invariants checked
#' @param to `incidence()` object we want
#'
#' @return TRUE or FALSE
#'
#' @noRd
incidence_can_reconstruct <- function(x, to) {

  x_names <- names(x)

  ## check groups are present
  groups <- attr(to, "groups")
  if (!is.null(groups)) {
    if (!(all(groups %in% x_names))) {
      return(FALSE)
    }
  }

  ## check count is present
  counts <- attr(to, "counts")
  if (!any(counts %in% x_names)) {
    return(FALSE)
  }

  ## check date is present
  date_var <- attr(to, "date")
  if (!all(date_var %in% x_names)) {
    return(FALSE)
  }


  ## ensure no rows are duplicated within x
  if (anyDuplicated(as.data.table(x))) {
    return(FALSE)
  }

  TRUE
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
#' Function to reconstruct object of incidence class
#'
#' Once you have encoded the invariant logic into incidence_can_reconstruct, we
#' need a second function that applies that check and either performs the actual
#' reconstruction, or falls back to a bare tibble.
#'
#' @param x x data.frame to have it's invariants checked
#' @param to object we want
#'
#' @noRd
incidence_reconstruct <- function(x, to) {
  if (incidence_can_reconstruct(x, to)) {
    df_reconstruct(x, to)
  } else {
    new_bare_tibble(x)
  }
}
# -------------------------------------------------------------------------



# -------------------------------------------------------------------------
# This function is a data frame specific helper.  Currently we are recommended
# to copy in to our own package but it may evenutally find it's way in to one of
# the tidy packages. See:
# https://github.com/DavisVaughan/2020-06-01_dplyr-vctrs-compat
df_reconstruct <- function(x, to) {
  attrs <- attributes(to)

  # Keep column and row names of `x`
  attrs$names <- names(x)
  attrs$row.names <- .row_names_info(x, type = 0L)

  # Otherwise copy over attributes of `to`
  attributes(x) <- attrs

  # fix names in case a count removed
  x_names <- names(x)
  counts <- attr(x, "counts")
  attr(x, "counts") <- counts[counts %in% x_names]

  x
}
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Need to define a few base R methods to ensure things work as expected

#' @export
`[.incidence2` <- function(x, i, j, ...) {
  out <- NextMethod()
  incidence_reconstruct(out, x)
}

#' @export
`[<-.incidence2` <- function(x, i, j, ..., value) {
  out <- NextMethod()
  incidence_reconstruct(out, x)
}

#' @export
`names<-.incidence2` <- function(x, value) {
  current_names <- names(x)

  date_var <- attr(x, "date")
  date_index <- which(current_names %in% date_var)
  attr(x, "date") <- value[date_index]

  count_var <- attr(x, "counts")
  count_index <- which(current_names %in% count_var)
  attr(x, "counts") <- value[count_index]

  group_vars <- attr(x, "groups")
  if (!is.null(group_vars)) {
    group_index <- which(current_names %in% group_vars)
    attr(x, "groups") <- value[group_index]
  }

  out <- NextMethod()
  incidence_reconstruct(out, x)
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Registered in `.onLoad()` in zzz.R
dplyr_reconstruct_incidence <- function(data, template) {
  incidence_reconstruct(data, template)
}
# -------------------------------------------------------------------------
