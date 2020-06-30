.onLoad <- function(...) {
  op <- options()
  op.incidence <- list(
    incidence.max.days = 18262,
    incidence.warn.first_date = TRUE
  )
  toset <- !names(op.incidence) %in% op
  if (any(toset)) options(op.incidence[toset])

  vctrs::s3_register("dplyr::dplyr_reconstruct", "incidence", method = dplyr_reconstruct_incidence)
}


