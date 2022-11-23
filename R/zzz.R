# nocov start
.onLoad <- function(...) {
    s3_register("dplyr::dplyr_reconstruct", "incidence")
    invisible()
}
# nocov end
