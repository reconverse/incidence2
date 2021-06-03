# nocov start
.onLoad <- function(...) {
  vctrs::s3_register(
   "dplyr::dplyr_reconstruct",
   "incidence",
   method = dplyr_reconstruct_incidence
  )
  invisible()
}
# nocov end
