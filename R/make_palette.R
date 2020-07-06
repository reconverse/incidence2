make_palette <- function(x, quiet = FALSE, suggest = NULL) {
  function(n) {
    if (!is.numeric(n)) stop("n is not a number")

    if (n <= length(x)) {
      x[seq_len(n)]
    } else {
      if (!quiet) {
        msg <- sprintf(
          paste("Using more colors (%d) than this palette can handle (%d);",
                "some colors will be interpolated."),
          n,
          length(x)
        )
        if (!is.null(suggest)) {
          msg <- paste0(
            msg,
            sprintf("\nConsider using `%s` palette instead?",
                    suggest)
          )
        }
        message(msg)
      }
      colorRampPalette(x)(n)
    }
  }
}
