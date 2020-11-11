arg_to_values <- function(args) {
    if (is.null(args)) {
        NULL
    } else if (length(args) == 1) {
        as.character(args)
    } else {
        as.character(args[-1])
    }
}