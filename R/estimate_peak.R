#' Estimate the peak date of an incidence curve using bootstrap
#'
#' This function can be used to estimate the peak of an epidemic curve stored as
#' `incidence`, using bootstrap. See [bootstrap()] for more information
#' on the resampling.
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}, with inputs on
#'   caveats from Michael Höhle.
#'
#' @details Input dates are resampled with replacement to form bootstrapped
#'   datasets; the peak is reported for each, resulting in a distribution of
#'   peak times. When there are ties for peak incidence, only the first date is
#'   reported.
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
#' @param x An `incidence` object.
#'
#' @param n The number of bootstrap datasets to be generated; defaults to 100.
#'
#' @param alpha The type 1 error chosen for the confidence interval; defaults to
#'   0.05.
#'
#' @return A list containing the following items:
#'
#' - `observed`: the peak incidence of the original dataset
#' - `estimated`: the mean peak time of the bootstrap datasets
#' - `ci`: the confidence interval based on bootstrap datasets
#' - `peaks`: the peak times of the bootstrap datasets
#'
#' @seealso [bootstrap()] for the bootstrapping underlying this
#'   approach and [find_peak()] to find the peak in a single
#'   `incidence` object.
#'
#' @examples
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#'   withAutoprint( {
#'     # load data and create incidence
#'     data(fluH7N9_china_2013, package = "outbreaks")
#'     i <- incidence(fluH7N9_china_2013, date_index = date_of_onset)
#'     i
#'
#'     # one simple bootstrap
#'     x <- bootstrap(i)
#'     x
#'
#'     # find 95% CI for peak time using bootstrap
#'     peak_data <- estimate_peak(i)
#'     peak_data
#'     summary(peak_data$peaks)
#'   })
#' }
#'
#' @export
estimate_peak <- function(x, n = 100, alpha = 0.05) {
  if (!inherits(x, "incidence")) {
    stop("x is not an incidence object")
  }

  count_var <- get_count_name(x)
  group_vars <- get_group_names(x)
  date_var <- get_date_name(x)

  if (length(group_vars) >= 1L) {
    msg <- "%s is stratified by groups; regrouping before finding peaks."
    message(sprintf(msg, deparse(substitute(x))))
    x <- regroup(x)
  }

  out <- list()

  ## use it to find CI for epidemic peak
  out$observed <- find_peak(x)

  ## peaks on 'n' bootstrap samples
  message("Estimating peaks from bootstrap samples:\n")
  pb <- utils::txtProgressBar(min = 0, max = n, style = 1)
  peak_boot <- lapply(1:n,
                      function(i) {
                        res <- find_peak(suppressMessages(bootstrap(x)))
                        utils::setTxtProgressBar(pb, i)
                        res
                      }
  )
  cat("\n\n")

  suppressMessages({
    ## convert to vector without losing Date class
    peak_boot <- dplyr::bind_rows(peak_boot)

    # store relevant stats
    out$estimated <- mean(peak_boot[[date_var]])
    QUANTILE <-
      if (inherits(peak_boot[[date_var]], c("Date", "POSIX"))) {
        quantile_Date
      } else {
        stats::quantile
      }

    out$ci <- QUANTILE(peak_boot[[date_var]], c(alpha / 2, 1 - alpha / 2))
    out$peaks <- peak_boot
    out
  })

}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# quantiles for Date objects
quantile_Date <- function(x, ...) {
  if (!inherits(x, "Date")) {
    stop("'x' is not a 'Date' object")
  }

  first_date <- min(x, na.rm = TRUE)
  x_num <- as.numeric(x - min(x))
  out <- stats::quantile(x_num, ...)
  first_date + out
}
# -------------------------------------------------------------------------
