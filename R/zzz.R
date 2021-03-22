# nocov start
.onLoad <- function(...) {
  vctrs::s3_register(
   "dplyr::dplyr_reconstruct",
   "incidence2",
   method = dplyr_reconstruct_incidence
  )
  invisible()
}


# nocov end
