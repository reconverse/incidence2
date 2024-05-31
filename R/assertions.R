.assert_bool <- function(x) {
    assert_bool(
        x,
        .arg = deparse(substitute(x)),
        .call = sys.call(-1L),
        .subclass = NULL
    )
}

# -------------------------------------------------------------------------
.assert_scalar_character <- function(x) {
    assert_scalar_character(
        x,
        .arg = deparse(substitute(x)),
        .call = sys.call(-1L),
        .subclass = NULL
    )
}

# -------------------------------------------------------------------------
.assert_scalar_numeric_not_na <- function(x) {
    assert_scalar_numeric_not_na(
        x,
        .arg = deparse(substitute(x)),
        .call = sys.call(-1L),
        .subclass = NULL
    )
}

# -------------------------------------------------------------------------
.assert_scalar_whole <- function(x) {
    assert_scalar_whole(
        x,
        .arg = deparse(substitute(x)),
        .call = sys.call(-1L),
        .subclass = NULL
    )
}
