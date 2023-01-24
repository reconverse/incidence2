#' Summary of an incidence object
#'
#' @param object An 'incidence' object.
#' @param ... Not used.
#'
#' @return object (invisibly).
#'
#' @examples
#'
#' data(ebola_sim_clean, package = "outbreaks")
#' dat <- ebola_sim_clean$linelist
#' inci <- incidence(dat, "date_of_onset", groups = c("gender", "hospital"))
#' summary(inci)
#'
#' @export
summary.incidence2 <- function(object, ...) {

    count_variable <- get_count_variable_name.incidence2(object)
    group_variables <- get_group_names.incidence2(object)
    date_variable <- get_date_index_name.incidence2(object)
    count_name <- get_count_value_name.incidence2(object)

    # range
    dates <- range(.subset2(object, date_variable))
    from  <- sprintf("From:          %s", format(dates[1]))
    to    <- sprintf("To:            %s", format(dates[2]))

    # group summary
    if (is.null(group_variables)) {
        groups_text <- "Groups:      NULL"
    } else {
        groups_text <- sprintf("Groups:        %s", paste(group_variables, collapse =", " ))
    }

    # observation summary
    tmp <- .subset(object, c(count_variable, group_variables, count_name))
    setDT(tmp)
    tmp <- tmp[, lapply(.SD, sum), by = c(group_variables, count_variable)]
    setDF(tmp)
    class(tmp) <- c("tbl", class(tmp))
    observations <- c("\nTotal observations:", format(tmp, n=nrow(tmp)))

    out <- c(from, to, groups_text, observations)
    writeLines(out)
    invisible(object)
}
