source("./data/corpora/irc26.R")
source("./data/corpora/ecfr26.R")
library(fs)

load_corpus <- function() {

  if(file_exists("./data/irc26.rda")) {
    tryCatch(
      load("./data/irc26.rda"),
      error = function(cond) {
      }
    )
  } else {
    message("'irc26.rda' does not seem to exist. Attempting to download and prepare...")
    extract_irc26()
    tryCatch(
      load("./data/irc26.rda"),
      error = function(cond) {
        message("Error loading 'irc26.rda' after attempting to download and prepare it!")
      }
    )
  }

  if(file_exists("./data/ecfr26.rda")) {
    tryCatch(
      load("./data/ecfr26.rda"),
      error = function(cond) {
      }
    )
  } else {
    message("'ecfr26.rda' does not seem to exist. Attempting to download and prepare...")
    extract_ecfr26()
    tryCatch(
      load("./data/ecfr26.rda"),
      error = function(cond) {
        message("Error loading 'ecfr26.rda' after attempting to download and prepare it!")
      }
    )
  }

  invisible(c(irc26, ecfr26))
}


