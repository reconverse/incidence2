#' Coerce to incidence
#'
#' `build_incidence()` coerces an object to an incidence of events.
#'
#' @param x An object to converse to `incidence`.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
build_incidence <- function(x, ...) UseMethod("build_incidence")

#' @rdname build_incidence
#' @export
build_incidence.default <- function(x, ...) {
  abort(sprintf("Not implemented for class %s", paste(class(x), collapse = ", ")))
}

#' @param date_index The time index(es) of the given data. Multiple inputs only
#'   make sense when x is a linelist, and in this situation, to avoid ambiguity,
#'   the vector must be named.  These names will be used for the resultant count
#'   columns.
#' @param groups An optional vector giving the names of the groups of
#'   observations for which incidence should be grouped.
#' @param counts The count variables of the given data.  If NULL (default) the
#'   data is taken to be a linelist of individual observations.
#' @param na_as_group A logical value indicating if missing group values (NA)
#'   should treated as a separate category (`TRUE`) or removed from
#'   consideration (`FALSE`). Defaults to `TRUE`.
#' @param FUN Function applied to the `dates_index` vectors before grouping. The
#'   first argument of `FUN` must work with a `dates_index` vector. Defaults to
#'   the identity function.
#' @param args List of additional arguments passed to FUN.
#'
#' @rdname build_incidence
#' @export
build_incidence.data.frame <- function(x, date_index, groups = NULL, counts = NULL,
                                    na_as_group = TRUE, FUN = identity,
                                    args = list(), ...) {

  # Convert date_index to character variables and facilitate renaming
  date_index <- enquo(date_index)
  idx <- tidyselect::eval_select(date_index, x)
  if (!length(idx)) abort("`date_index` must have length greater than zero")
  if (length(idx) > 1) {
    call_nms <- call_args_names(get_expr(date_index))
    if (any(call_nms %in% "")) {
      abort("If multiple date indices are specified they must be named")
    }
    names(x)[idx] <- date_index <- names(idx)
  } else {
    idx <- tidyselect::eval_select(date_index, x, allow_rename = FALSE)
    date_index <- names(x)[idx]
  }

  # Convert groups to character variables
  groups_quo <- enquo(groups)
  if (!quo_is_null(groups_quo)) {
    idx <- tidyselect::eval_select(groups_quo, x, allow_rename = FALSE)
    groups <- names(x)[idx]
  } else {
    groups <- NULL
  }

  # Convert counts to character variables
  counts_quo <- enquo(counts)
  if (!quo_is_null(counts_quo)) {
    idx <- tidyselect::eval_select(counts_quo, x, allow_rename = FALSE)
    counts <- names(x)[idx]
  } else {
    counts <- NULL
  }

  # generate names for resultant count columns if needed
  if (is.null(counts)) {
    count_names <- if (length(date_index) == 1) "count" else date_index
  } else if (length(date_index) > 1) {
    abort("If `counts` is specified `date_index` must be of length 1")
  }

  # check all date_index are of same class
  date_classes <- vapply(x[date_index], function(x) class(x)[1], character(1))
  if (length(unique(date_classes)) != 1L) {
    abort("date_index columns must be of the same class")
  }

  # Apply function to date_index columns
  x[date_index] <- mapply(FUN = FUN, x[date_index], MoreArgs = args, SIMPLIFY = FALSE)

  # Calculate an incidence object for each value of date_index
  dt <- !any(vapply(x, typeof, character(1)) == "list")
  res <-
    lapply(
      seq_along(date_index),
      function(i) {
        calculate_incidence(
          x = x,
          date_index = date_index[i],
          groups = groups,
          counts = counts,
          count_name = count_names[i],
          na_as_group = na_as_group,
          dt = dt
        )
      }
    )

  # if there is only 1 value for date_index we can just return the entry,
  # otherwise we need to merge the results
  if (length(date_index) == 1) {
    res <- res[[1]]
  } else {
    res <- Reduce(
      function(x, y) full_join(x, y, by = c("date_index", groups)),
      res
    )
  }

  if (is.null(counts)) counts <- count_names
  setnafill(res, fill = 0, cols = counts)
  res <- res[order(res$date_index), ]

  new_incidence(res, date = "date_index", groups = groups, counts = counts)
}


calculate_incidence <- function(x, date_index, groups, counts, count_name, na_as_group, dt) {

  # Remove missing observations
  n_orig <- nrow(x)
  x <- x[!is.na(x[[date_index]]), , drop=FALSE]
  n_new <- nrow(x)
  if (n_new < n_orig) {
    message(sprintf("%d missing observations were removed.", n_orig - n_new))
  }

  # generate grouped_dates
  if (dt) {
    setDT(x)
    if (is.null(counts)) {
      x <- x[, .N, keyby = c(date_index, groups)]
      setnames(x, length(x), count_name)
    } else {
      x <- x[, lapply(.SD, sum, na.rm = TRUE), keyby = c(date_index, groups), .SDcols = counts]
    }
    setDF(x)

  } else {
    x <- grouped_df(x, c(date_index, groups))
    if (is.null(counts)) {
      x <- summarise(x, count__ = n(), .groups = "drop")
      colnames(x)[length(x)] <- count_name
    } else {
      x <- summarise(x, across(all_of(counts), ~sum(., na.rm = TRUE)), .groups = "drop")
    }
  }

  # set name for date column
  date_col <- "date_index"

  # give date column correct name
  colnames(x)[1] <- date_col

  # filter out NA groups if desired
  if (!na_as_group) {
    x <- x[complete.cases(x[, groups, drop = FALSE]), , drop = FALSE]
  }

  # reorder (dates, groups, counts)
  if (is.null(counts)) counts <- count_name
  x[c(date_col, groups, counts)]
}
