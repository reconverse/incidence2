incidence_reconstructable <- function(x, to) {

  x_names <- names(x)

  # check groups are present
  groups <- attr(to, "groups")
  if (!is.null(groups)) {
    if (!(all(groups %in% x_names))) {
      return(FALSE)
    }
  }

  # check count is present
  count <- attr(to, "count")
  if (!(count %in% x_names)) {
    return(FALSE)
  }

  # check date is present
  date_var <- attr(to, "date")
  if (!all(date_var %in% x_names)) {
    return(FALSE)
  }

  # TODO - should we check class?

  # ensure no rows are duplicated within x
  if (anyDuplicated(x)) {
    return(FALSE)
  }

  # check interval is the same or a multiple off
  to_interval <- get_interval(to)
  x_intervals <- unique(diff(x[[date_var[1]]]))
  if (!(all((x_intervals %% to_interval) == 0))) {
    return(FALSE)
  }

  TRUE
}



incidence_reconstruct <- function(x, to) {
  if (incidence_reconstructable(x, to)) {
    df_reconstruct(x, to)
  } else {
    new_bare_tibble(x)
  }
}



df_reconstruct <- function(x, to) {
  attrs <- attributes(to)

  # Keep column and row names of `x`
  attrs$names <- names(x)
  attrs$row.names <- .row_names_info(x, type = 0L)

  # Otherwise copy over attributes of `to`
  attributes(x) <- attrs

  x
}


new_bare_tibble <- function(x) {
  # Strips all attributes off `x` since `new_tibble()` currently doesn't
  x <- vctrs::new_data_frame(x)
  tibble::new_tibble(x, nrow = nrow(x))
}

#' @export
`[.incidence` <- function(x, i, j, ...) {
  out <- NextMethod()
  incidence_reconstruct(out, x)
}

#' @export
`[<-.incidence` <- function(x, i, j, ..., value) {
  out <- NextMethod()
  incidence_reconstruct(out, x)
}

#' @export
`names<-.incidence` <- function(x, value) {

  current_names <- names(x)

  date_var <- attr(x, "date")
  date_index <- which(current_names %in% date_var)
  attr(x, "date") <- value[date_index]

  count_var <- attr(x, "count")
  count_index <- which(current_names %in% count_var)
  attr(x, "count") <- value[count_index]

  group_vars <- attr(x, "groups")
  if (!is.null(group_vars)) {
    group_index <- which(current_names %in% group_vars)
    attr(x, "groups") <- value[group_index]
  }

  out <- NextMethod()
  incidence_reconstruct(out, x)
}



dplyr_reconstruct.incidence <- function(data, template) {
  incidence_reconstruct(data, template)
}


# Registered in `.onLoad()`
dplyr_reconstruct_incidence <- function(data, template) {
  incidence_reconstruct(data, template)
}
