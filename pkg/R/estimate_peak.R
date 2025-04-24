#' Estimate the peak date of an incidence curve
#'
# -------------------------------------------------------------------------
#' This function can be used to estimate the peak of an epidemic curve using
#' bootstrapped samples of the available data.
#'
# -------------------------------------------------------------------------
#' @details
#'
#' Input dates are resampled with replacement to form bootstrapped datasets;
#' the peak is reported for each, resulting in a distribution of peak times.
#' When there are ties for peak incidence, only the first date is reported.
#'
#' Note that the bootstrapping approach used for estimating the peak time makes
#' the following assumptions:
#'
#' - the total number of event is known (no uncertainty on total incidence)
#' - dates with no events (zero incidence) will never be in bootstrapped
#'   datasets
#' - the reporting is assumed to be constant over time, i.e. every case is
#'   equally likely to be reported
#'
# -------------------------------------------------------------------------
#' @param x An [incidence2][incidence2::incidence] object.
#'
#' @param n `integer`.
#'
#' The number of bootstrap datasets to be generated; defaults to 100.
#'
#' `[double]` vectors will be converted via `as.integer(n)`.
#'
#' @param alpha `numeric`.
#'
#' The type 1 error chosen for the confidence interval; defaults to 0.05.
#'
#' @param first_only `bool`.
#'
#' Should only the first peak (by date) be kept.
#'
#' Defaults to `TRUE`.
#'
#' @param progress `bool`.
#'
#' Should a progress bar be displayed (default = TRUE)
#'
# -------------------------------------------------------------------------
#' @return
#'
#' A data frame with the the following columns:
#'
#' - `observed_date`: the date of peak incidence of the original dataset.
#' - `observed_count`: the peak incidence of the original dataset.
#' - `estimated`: the median peak time of the bootstrap datasets.
#' - `lower_ci/upper_ci`: the confidence interval based on bootstrap datasets.
#' - `bootstrap_peaks`: a nested tibble containing the the peak times of the
#'   bootstrapped datasets.
#'
# -------------------------------------------------------------------------
#' @seealso
#'
#' [`bootstrap_incidence()`] for the bootstrapping underlying this approach and
#' [`keep_peaks()`] to get the peaks in a single
#' [incidence2][incidence2::incidence] object.
#'
# -------------------------------------------------------------------------
#' @author
#'
#' Thibaut Jombart and Tim Taylor, with inputs on caveats from Michael Höhle.
#'
# -------------------------------------------------------------------------
#' @examples
#' \dontshow{.old <- data.table::setDTthreads(2)}
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#'
#'   # load data and create incidence
#'   data(fluH7N9_china_2013, package = "outbreaks")
#'   i <- incidence(fluH7N9_china_2013, date_index = "date_of_onset")
#'
#'   # find 95% CI for peak time using bootstrap
#'   estimate_peak(i)
#' }
#' \dontshow{data.table::setDTthreads(.old)}
#'
# -------------------------------------------------------------------------
#' @aliases estimate_peaks
#'
# -------------------------------------------------------------------------
#' @export
estimate_peak <- function(x, n = 100L, alpha = 0.05, first_only = TRUE, progress = TRUE) {

    if (!inherits(x, "incidence2"))
        .stopf("`%s` is not an 'incidence2' object.", deparse(substitute(x)))

    assert_scalar_whole(n, .subclass = "incidence2_error")
    assert_scalar_numeric_not_na(alpha, .subclass = "incidence2_error")

    assert_bool(first_only, .subclass = "incidence2_error")
    assert_bool(progress, .subclass = "incidence2_error")

    # Needed for CRAN/data.table
    ..date_var <- bootstrap_peaks <- ..observed_peak <- observed_count <- . <- i.bootstrap_peaks <- NULL

    # get relevant column names
    date_var <- get_date_index_name(x)
    count_var <- get_count_variable_name(x)
    count_val <- get_count_value_name(x)
    group_vars <- get_group_names(x)
    grouping_variables <- c(group_vars, count_var)

    # Calculate the observed peak and convert to data.table for later
    observed_peak <- keep_peaks(x, first_only = first_only)
    setDT(observed_peak)
    if (!first_only)
        observed_peak <- observed_peak[, lapply(.SD, list), by = c(grouping_variables, count_val)]

    # Calculate peaks from bootstrapped data samples with optional progress bar
    if (interactive() && progress) {
        out <- vector("list", n)
        message("Estimating peaks from bootstrap samples:")
        pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
        for (i in seq_len(n)) {
            out[[i]] <- keep_peaks(bootstrap_incidence(x), first_only = first_only)
            utils::setTxtProgressBar(pb, i)
        }
        close(pb)
        cat("\n")
    } else {
        out <- lapply(
            seq_len(n),
            function(y) keep_peaks(bootstrap_incidence(x), first_only = first_only)
        )
    }
    out <- rbindlist(out)
    set(out, j = count_val, value = NULL)

    # TODO - check this with Thibaut
    # specify probabilities (lower_ci, median, upper_ci)
    probs <- c(alpha / 2, 0.5, 1 - alpha / 2)

    # group peaks by group_vars and count_var
    peaks <- out[, .(bootstrap_peaks = list(.SD)), by = grouping_variables]

    # group quantiles by group_vars count variable

    quantiles_list <- function(x, probs) {
        res <- stats::quantile(x, probs = probs, names = FALSE, type = 1)
        as.list(res)
    }

    quantiles <- out[,
                     stats::setNames(
                         Reduce(c, lapply(.SD, quantiles_list, probs = probs)),
                         c("lower_ci", "median", "upper_ci")
                     )
                     , .SDcols = date_var
                     , keyby = grouping_variables]

    # join peaks on to quantiles and observed peak
    quantiles[peaks, on = grouping_variables, bootstrap_peaks := i.bootstrap_peaks]


    expr <- lapply(c(date_var, count_val), as.name)
    names(expr) <- c("observed_peak", "observed_count")
    expr <- as.call(c(quote(`:=`), expr))
    quantiles[observed_peak, eval(expr), on = c(grouping_variables)]


    # reorder
    setcolorder(
        quantiles,
        c(group_vars, count_var, "observed_peak", "observed_count", "bootstrap_peaks")
    )

    # convert nested data.tables to data frames
    if (nrow(quantiles) > 1L) {
        quantiles[, bootstrap_peaks := lapply(bootstrap_peaks, as.data.frame)]
    } else {
        quantiles[, bootstrap_peaks := lapply(bootstrap_peaks, function(x) list(as.data.frame(x)))]
    }

    setDF(quantiles)

    # return as tibble
    tibble::as_tibble(quantiles)
}
