#' @export
rbind.incidence2 <- function(..., deparse.level = 1) {
    dat <- list(...)
    first <- dat[[1L]]
    compatible <- vapply(dat, inherits, TRUE, what = "data.frame")
    if (!all(compatible))
        stop("Incompatible inputs")
    dat <- lapply(dat, as.data.frame)
    out <- do.call(rbind, dat)
    .incidence_reconstruct(out, first)
}
