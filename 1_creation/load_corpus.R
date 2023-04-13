source("./data/corpora/irc26.R")
source("./data/corpora/ecfr26.R")
source("./data/corpora/courtlistener.R")
source("./data/corpora/caselaw.R")
source("./data/corpora/irs_bulletins.R")
source("./data/corpora/irs_instructions.R")
source("./data/corpora/irs_publications.R")

library(fs)
library(tibble)
library(arrow)

rds_dir <- "/data/rstudio/rds"

load_corpus <- function() {

  # first check if the corpus has already been prepared
  # If so, load it and return it
  rds_dir <- "/data/rstudio/rds"
  # rds_path <- path(rds_dir, "corpus.rds")
  pq_path <- path(rds_dir, "corpus.pq")
  
  if(exists("corpus")) {
    return(invisible(corpus))
  }
  
  if(file_exists(pq_path)) {
    message(str_glue("'{pq_path}' exists on disk. Loading and returning."))
    # corpus <- readRDS(pq_path)
    corpus <- read_parquet(pq_path) %>% pull(lines)
    return(invisible(corpus))
  }
  
  # not on disk. So load and prepare each individual data source
  
  ### Load IRC26 ###
  irc26_path <- path(rds_dir, "irc26.rds")
  
  if(file_exists(irc26_path)) {
    tryCatch(
      irc26 <- readRDS(irc26_path),
      error = function(cond) {
        message(str_glue("'{irc26_path}' exists, but there was an error loading it."))
      }
    )
  } else {
    message(str_glue("'{irc26_path'} does not seem to exist. Attempting to download and prepare it..."))
    irc26 <- extract_irc26()
  }

  ### Load eCFR26 ###
  ecfr26_path <- path(rds_dir, "ecfr26.rds")
  
  if(file_exists(ecfr26_path)) {
    tryCatch(
      ecfr26 <- readRDS(ecfr26_path),
      error = function(cond) {
        message(str_glue("'{ecfr26_path}' exists, but there was an error loading it."))
      }
    )
  } else {
    message(str_glue("'{ecfr26_path'} does not seem to exist. Attempting to download and prepare it..."))
    ecfr26 <- extract_ecfr26() 
  }

  ### Load CourtListener cases ###
  clist_path <- path(rds_dir, "courtlistener.rds")
  
  if(file_exists(clist_path)) {
    tryCatch(
      courtlistener <- readRDS(clist_path),
      error = function(cond) {
        message(str_glue("'{clist_path}' exists, but there was an error loading it."))
      }
    )
  } else {
    message(str_glue("'{clist_path'} does not seem to exist. Attempting to download and prepare it..."))
    courtlistener <- extract_courtlistener()
  }

  ### Load case.law cases ###
  claw_path <- path(rds_dir, "caselaw.rds")
  
  if(file_exists(claw_path)) {
    tryCatch(
      caselaw <- readRDS(claw_path),
      error = function(cond) {
        message(str_glue("'{claw_path}' exists, but there was an error loading it."))
      }
    )
  } else {
    message(str_glue("'{claw_path'} does not seem to exist. Attempting to download and prepare it..."))
    caselaw <- extract_caselaw()
  }

  ### Load IRS Bulletins ###
  bulletins_path <- path(rds_dir, "bulletins.rds")
  
  if(file_exists(bulletins_path)) {
    tryCatch(
      bulletins <- readRDS(bulletins_path),
      error = function(cond) {
        message(str_glue("'{bulletins_path}' exists, but there was an error loading it."))
      }
    )
  } else {
    message(str_glue("'{bulletins_path'} does not seem to exist. Attempting to download and prepare it..."))
    bulletins <- extract_bulletins()
  }

  ### Load IRS Publications ###
  publications_path <- path(rds_dir, "publications.rds")
  
  if(file_exists(publications_path)) {
    tryCatch(
      pubs <- readRDS(publications_path),
      error = function(cond) {
        message(str_glue("'{publications_path}' exists, but there was an error loading it."))
      }
    )
  } else {
    message(str_glue("'{publications_path'} does not seem to exist. Attempting to download and prepare it..."))
    pubs <- extract_publications()
  }
  
  ### Load IRS Instructions ###
  instructions_path <- path(rds_dir, "instructions.rds")
  
  if(file_exists(instructions_path)) {
    tryCatch(
      instructions <- readRDS(instructions_path),
      error = function(cond) {
        message(str_glue("'{instructions_path}' exists, but there was an error loading it."))
      }
    )
  } else {
    message(str_glue("'{instructions_path'} does not seem to exist. Attempting to download and prepare it..."))
    instructions <- extract_instructions()
  }
  
  corpus <- c(
    irc26,
    ecfr26,
    courtlistener,
    caselaw,
    bulletins,
    pubs,
    instructions
  )
  
  write_parquet(tibble(lines = corpus), pq_path)
  # saveRDS(corpus, rds_path, compress = FALSE)
  
  return(invisible(corpus))
}


