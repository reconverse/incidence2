.stop <- function(msg, ..., .call = sys.call(-1L), .subclass = NULL) {
    stop(errorCondition(msg, ..., class = .subclass, call = .call[1L]))
}

.assert_not_missing <- function(x, ..., .arg, .call, .subclass) {
    if (missing(x)) {
        msg <- sprintf("argument `%s` is missing, with no default.", .arg)
        .stop(msg, .call = .call, .subclass = .subclass)
    }
}

.assert_bool <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)
    if (!(is.logical(x) && length(x) == 1L) || is.na(x)) {
        msg <- sprintf("`%s` must be boolean (TRUE/FALSE).", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }
}

# -------------------------------------------------------------------------
.assert_scalar_character <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)
    if (!(is.character(x) && length(x) == 1L)) {
        msg <- sprintf("`%s` must be a character vector of length 1.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }
}

# -------------------------------------------------------------------------
.assert_scalar_numeric_not_na <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)
    if (!(is.numeric(x) && length(x) == 1L) || is.na(x)) {
        msg <- sprintf("`%s` must be a numeric vector of length 1 and not NA.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }
}

# -------------------------------------------------------------------------
.assert_scalar_whole <- function(
    x,
    ...,
    .arg = deparse(substitute(x)),
    .call = sys.call(-1L),
    .subclass = NULL
) {
    .assert_not_missing(x, ..., .arg = .arg, .call = .call, .subclass = .subclass)
    if (!.is_scalar_whole(x)) {
        msg <- sprintf("`%s` must be integerish and of length 1.", .arg)
        .stop(msg, ..., .call = .call, .subclass = .subclass)
    }
}
