#' Bootstrap incidence time series
#'
#' This function can be used to bootstrap `incidence` objects. Bootstrapping is
#' done by sampling with replacement the original input dates. See `details` for
#' more information on how this is implemented.
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}, Tim Taylor
#'
#' @export
#'
#' @details As original data are not stored in `incidence` objects, the
#'   bootstrapping is achieved by multinomial sampling of date bins weighted by
#'   their relative incidence.
#'
#' @param x An `incidence` object.
#'
#' @param randomise_groups A `logical` indicating whether groups should be
#'   randomised as well in the resampling procedure; respective group sizes will
#'   be preserved, but this can be used to remove any group-specific temporal
#'   dynamics. If `FALSE` (default), data are resampled within groups.
#'
#' @return An `incidence` object.
#'
#' @seealso [find_peak()] to use estimate peak date using bootstrap
bootstrap <- function(x, randomise_groups = FALSE) {
  if (!inherits(x, "incidence")) {
    stop("x is not an incidence object")
  }

  count_var <- get_count_vars(x)
  group_vars <- get_group_vars(x)
  date_vars <- get_date_vars(x)


  tbl <- select(x, !all_of(count_var))
  tbl <- dplyr::slice_sample(
    tbl,
    n = sum(x[[count_var]]),
    weight_by = x[[count_var]],
    replace = TRUE
  )

  tbl <- group_by(tbl, across(all_of(c(date_vars, group_vars))))
  tbl <- summarise(tbl, {{count_var}} := n(), .groups = "drop")

  if (randomise_groups) {
    for (gr in group_vars) {
      tbl[[gr]] <- sample_(tbl[[gr]])
    }
  }

  # create subclass of tibble
  tbl <- tibble::new_tibble(tbl,
                            groups = group_vars,
                            date = date_vars,
                            count = count_var,
                            interval = attr(x, "interval"),
                            cumulative = attr(x, "cumulative"),
                            nrow = nrow(tbl),
                            class = "incidence"
  )
  tibble::validate_tibble(tbl)
}



# A fix for the behaviour of `sample` when first argument is of length 1.
sample_ <- function(x, ...) {
  x[sample.int(length(x), ...)]
}
