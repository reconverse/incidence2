error_types <- new.env()

# TODO - think about turning this on
#default <- function(x) c(x, "incidence2_error")
#error_types$argument <- default("incidence2_argument_error")
#error_types$suggested <- default("incidence2_suggested_error")

error_types$argument <- error_types$suggested <- NULL
