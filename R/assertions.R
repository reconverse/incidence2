.assert_bool <- function(x) {
    ympes::.assert_bool(
        x,
        .arg = deparse(substitute(x)),
        .call = sys.call(-1L),
        .subclass = error_types$argument
    )
}

.assert_scalar_character <- function(x) {
    ympes::.assert_scalar_character(
        x,
        .arg = deparse(substitute(x)),
        .call = sys.call(-1L),
        .subclass = error_types$argument
    )
}

.assert_scalar_numeric_not_na <- function(x) {
    ympes::.assert_scalar_numeric_not_na(
        x,
        .arg = deparse(substitute(x)),
        .call = sys.call(-1L),
        .subclass = error_types$argument
    )
}

.assert_scalar_whole <- function(x) {
    ympes::.assert_scalar_whole(
        x,
        .arg = deparse(substitute(x)),
        .call = sys.call(-1L),
        .subclass = error_types$argument
    )
}

