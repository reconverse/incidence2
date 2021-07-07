# nocov start
.onLoad <- function(...) {
  vctrs::s3_register(
   "dplyr::dplyr_reconstruct",
   "incidence_df",
   method = dplyr_reconstruct_incidence_df
  )
  invisible()
}
# nocov end
