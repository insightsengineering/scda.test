test_that("lsimh01", {
  skip_if_not_installed("envsetup")

  normalize_rtf_snapshot <- function(path) {
    text <- paste(readLines(path, warn = FALSE, encoding = "latin1"), collapse = "\n")
    text <- gsub("\\\\'[[:xdigit:]]{2}", " ", text, perl = TRUE)
    text <- gsub("\\\\[[:alpha:]]+-?[[:digit:]]* ?", " ", text, perl = TRUE)
    text <- gsub("[{}]", " ", text, perl = TRUE)
    text <- gsub("[[:space:]]+", " ", text, perl = TRUE)
    trimws(text)
  }

  compare_normalized_rtf <- function(old, new) {
    identical(normalize_rtf_snapshot(old), normalize_rtf_snapshot(new))
  }
})
