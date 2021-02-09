# nocov start
.onLoad <- function(...) {
  vctrs::s3_register("ggplot2::scale_type", "yrwk")
  vctrs::s3_register("ggplot2::scale_type", "yrmon")
  vctrs::s3_register("ggplot2::scale_type", "yrqtr")
  vctrs::s3_register("ggplot2::scale_type", "yr")
  vctrs::s3_register("ggplot2::scale_type", "period")
  vctrs::s3_register(
   "dplyr::dplyr_reconstruct",
   "incidence2",
   method = dplyr_reconstruct_incidence
  )
  invisible()
}


# nocov end
