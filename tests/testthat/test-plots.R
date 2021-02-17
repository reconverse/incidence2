# setup -------------------------------------------------------------------
firstday <- as.Date("2020-01-01") # Wednesday
lastday <- as.Date("2021-12-31")  # Friday
dates <- seq.Date(from = firstday, to = lastday, by = "day")
count <- c(rep(1L, 366), rep(2L, 365))
dat <- data.frame(date = dates, count = count)

save_png <- function(code, width = 400, height = 400) {
  path <- tempfile(fileext = ".png")
  png(path, width = width, height = height)
  on.exit(dev.off())
  print(code)
  path
}

expect_snapshot_plot <- function(name, code) {
  skip_if_not_installed("ggplot2", "2.0.0")
  skip_on_os(c("windows", "mac"))
  path <- save_png(code)
  expect_snapshot_file(path, paste0(name, ".png"))
}

test_that("day plotting works", {
  x <- incidence(dat, date_index = date)
  expect_snapshot_plot("day", plot(x))
})

