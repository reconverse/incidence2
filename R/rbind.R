#' @export
rbind.incidence2 <- function (..., deparse.level = 1) {
    dat <- list(...)
    first <- dat[[1L]]
    compatible <- sapply(dat, inherits, "data.frame")
    if (!all(compatible))
        stop("Incompatible inputs")
    dat <- lapply(dat, as.data.frame)
    out <- do.call(rbind, dat)
    .incidence_reconstruct(out, first)
}
