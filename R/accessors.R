#' Access various elements of an incidence object
#'
# -------------------------------------------------------------------------
#' @param x An \R object.
#'
#' @param ... Not currently used.
#'
# -------------------------------------------------------------------------
#' @return
#'
#'   - `get_date_index_name()`: The name of the date_index variable of x.
#'   - `get_dates_name()`: Alias for `get_date_index_name()`.
#'   - `get_count_variable_name()`: The name of the count variable of x.
#'   - `get_count_value_name()`: The name of the count value of x.
#'   - `get_group_names()`: The name(s) of the group variable(s) of x.
#'
#'   - `get_date_index()`: The date_index variable of x.
#'   - `get_dates()`: Alias for `get_date_index()`.
#'   - `get_count_variable()`: The count variable of x.
#'   - `get_count_value()`: The count value of x.
#'   - `get_groups()`: List of the group variable(s) of x.
#'
# -------------------------------------------------------------------------
#' @examples
#' \dontshow{data.table::setDTthreads(2)}
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#' \dontshow{withAutoprint(\{}
#'     data(ebola_sim_clean, package = "outbreaks")
#'     dat <- ebola_sim_clean$linelist
#'     i <- incidence(dat, date_index = "date_of_onset",
#'                    groups = c("gender", "hospital"))
#'
#'     get_count_variable_name(i)
#'     get_group_names(i)
#'     get_dates_name(i)
#' \dontshow{\})}
#' }
#'
# -------------------------------------------------------------------------
#' @name accessors
NULL

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

#' @rdname accessors
#' @export
get_date_index_name <- function(x, ...) {
    UseMethod("get_date_index_name")
}

# -------------------------------------------------------------------------
#' @rdname accessors
#' @export
get_date_index_name.default <- function(x, ...) {
    stopf("Not implemented for class [%s].", paste(class(x), collapse = ", "))
}

# -------------------------------------------------------------------------
#' @rdname accessors
#' @export
get_date_index_name.incidence2 <- function(x, ...) {
    attr(x, "date_index")
}

# -------------------------------------------------------------------------
#' @rdname accessors
#' @export
get_dates_name <- get_date_index_name

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

#' @rdname accessors
#' @export
get_count_variable_name <- function(x, ...) {
    UseMethod("get_count_variable_name")
}

# -------------------------------------------------------------------------
#' @rdname accessors
#' @export
get_count_variable_name.default <- function(x, ...) {
    stopf("Not implemented for class [%s].", paste(class(x), collapse = ", "))
}

# -------------------------------------------------------------------------
#' @rdname accessors
#' @export
get_count_variable_name.incidence2 <- function(x, ...) {
    attr(x, "count_variable")
}

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

#' @rdname accessors
#' @export
get_count_value_name <- function(x, ...) {
    UseMethod("get_count_value_name")
}

# -------------------------------------------------------------------------
#' @rdname accessors
#' @export
get_count_value_name.default <- function(x, ...) {
    stopf("Not implemented for class [%s].", paste(class(x), collapse = ", "))
}

# -------------------------------------------------------------------------
#' @rdname accessors
#' @export
get_count_value_name.incidence2 <- function(x, ...) {
    attr(x, "count_value")
}

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

#' @rdname accessors
#' @export
get_group_names <- function(x, ...) {
    UseMethod("get_group_names")
}

# -------------------------------------------------------------------------
#' @rdname accessors
#' @export
get_group_names.default <- function(x, ...) {
    stopf("Not implemented for class [%s].", paste(class(x), collapse = ", "))
}

# -------------------------------------------------------------------------
#' @rdname accessors
#' @export
get_group_names.incidence2 <- function(x, ...) {
    attr(x, "groups")
}

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
#' @rdname accessors
#' @export
get_date_index <- function(x, ...) {
    UseMethod("get_date_index")
}

# -------------------------------------------------------------------------
#' @rdname accessors
#' @export
get_date_index.default <- function(x, ...) {
    stopf("Not implemented for class [%s].", paste(class(x), collapse = ", "))
}

# -------------------------------------------------------------------------
#' @rdname accessors
#' @export
get_date_index.incidence2 <- function(x, ...) {
    .subset2(x, attr(x, "date_index"))
}

# -------------------------------------------------------------------------
#' @rdname accessors
#' @export
get_dates <- get_date_index

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

#' @rdname accessors
#' @export
get_count_variable <- function(x, ...) {
    UseMethod("get_count_variable")
}

# -------------------------------------------------------------------------
#' @rdname accessors
#' @export
get_count_variable.default <- function(x, ...) {
    stopf("Not implemented for class [%s].", paste(class(x), collapse = ", "))
}

# -------------------------------------------------------------------------
#' @rdname accessors
#' @export
get_count_variable.incidence2 <- function(x, ...) {
    .subset2(x, attr(x, "count_variable"))
}

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

#' @rdname accessors
#' @export
get_count_value <- function(x, ...) {
    UseMethod("get_count_value")
}

# -------------------------------------------------------------------------
#' @rdname accessors
#' @export
get_count_value.default <- function(x, ...) {
    stopf("Not implemented for class [%s].", paste(class(x), collapse = ", "))
}

# -------------------------------------------------------------------------
#' @rdname accessors
#' @export
get_count_value.incidence2 <- function(x, ...) {
    .subset2(x, attr(x, "count_value"))
}

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

#' @rdname accessors
#' @export
get_groups <- function(x, ...) {
    UseMethod("get_groups")
}

# -------------------------------------------------------------------------
#' @rdname accessors
#' @export
get_groups.default <- function(x, ...) {
    stopf("Not implemented for class [%s].", paste(class(x), collapse = ", "))
}

# -------------------------------------------------------------------------
#' @rdname accessors
#' @export
get_groups.incidence2 <- function(x, ...) {
    .subset(x, attr(x, "groups"))
}
