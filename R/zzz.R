# nocov start
.onLoad <- function(...) {
    s3_register("dplyr::dplyr_reconstruct", "incidence2")
    invisible()
}
# nocov end
