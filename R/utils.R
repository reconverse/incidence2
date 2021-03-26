# check for suggested packages --------------------------------------------
check_suggests <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    msg <- sprintf("Suggested package '%s' not present.", package)
    stop(msg, call. = FALSE)
  }
}


# check if entries of a vector are whole numbers
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}


# cast a vector to an integer
int_cast <- function(x) {
  x <- unclass(x)
  if (!all(is.wholenumber(x) | is.na(x))) {
    msg <- paste(deparse1(substitute(x)), "must be a vector of whole numbers")
    stop(msg, call. = FALSE)
  }
  res <- as.integer(x)
  names(res) <- names(x)
  res
}


get_interval_number <- function(x) {
  if (!grepl("^\\d", x)) return(1L)
  as.integer(gsub("^(\\d*).*$", "\\1", x))
}


get_interval_type <- function(x) {

  if (!is.character(x)) {
    return(typeof(x))
  }

  day <- "^\\s*days?\\s*$|\\sdays?\\s+|\\sdays?\\s*$"
  if (grepl(day, x, ignore.case = TRUE)) {
    return("day")
  } else if (grepl("week", x, ignore.case = TRUE)) {
    return("week")
  }  else if (grepl("month", x, ignore.case = TRUE)) {
    return("month")
  } else if (grepl("quarter", x, ignore.case = TRUE)) {
    return("quarter")
  } else if (grepl("year", x, ignore.case = TRUE)) {
    return("year")
  }  else {
    return("day")
  }
}
