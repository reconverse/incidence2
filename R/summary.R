#' Summary of an incidence object
#'
#' @param x An 'incidence' object.
#' @param ... Not used.
#'
#' @return x (invisibly).
#'
#' @export
summary.incidence <- function(x, ...) {

  ..count_var <- . <- NULL # due to NSE notes in R CMD check

  count_var <- get_count_names(x)
  groups <- get_group_names(x)

  # general overview text without header
  out <- overview(x)[-1]

  if (inherits(x, "incidence2")) {
    inter <- interval(x)
    timespan <- sprintf("timespan: %d days", get_timespan(x))
    out <- c(out, inter, timespan)
  }

  # information about groups
  if (!is.null(groups)) {
    groups_text <- sprintf(
      "%d grouped %s",
      length(groups),
      ifelse(length(groups) < 2, "variable\n", "variables\n")
    )

    dt <- !any(vapply(x, typeof, character(1)) == "list")
    tables <-
      if (dt) {
        lapply(
          groups,
          function(gr) {
            tmp <- as.data.table(x)
            tmp <- tmp[, lapply(.SD, sum, na.rm = TRUE), by = c(gr), .SDcols = count_var]
            tmp <- tibble::as_tibble(tmp)
            c(format(tmp)[-1], "\n")
          }
        )
      } else {
        lapply(
          groups,
          function(gr) {
            tmp <- grouped_df(x, gr)
            tmp <- summarise(tmp, across(all_of(count_var), ~sum(., na.rm = TRUE)), .groups = "drop")
            c(format(tmp)[-1], "\n")
          }
        )
      }
    out <- c(out, "", groups_text, unlist(tables))
  }

  writeLines(c("", out))
  invisible(x)
}
