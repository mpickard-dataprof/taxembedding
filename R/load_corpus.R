load_corpus <- function() {

  load("data/irc26.rda")
  load("data/ecfr26.rda")
  invisible(c(irc26, ecfr26))
}


