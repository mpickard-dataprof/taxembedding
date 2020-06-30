## I'm using the PCC
library(readr) # same as import in python
library(dplyr)
library(stringi)
library(stringr)


#text <- read_lines("./R/sysdata.rda/test.txt")
text <- read_lines("./R/sysdata.rda/usc26.pre")

# this function applies to the PCC/PRE file format
# bell character = HEX 07 : regex = "\\x07"
remove_bell_characters <- function(line) {
  patterns <- c(
    "\\x07(F|S)\\d{4,5}",        # Bell F and S codes have 4-5 digit arguments
    "\\x07I\\d{2}",              # Bell I codes have two digit arguments
    "\\x07T\\d",                 # Bell T codes have one digit arguments
    "\\x07G\\d",                 # Bell G codes have one digit arguments
    "\\x07g\\g{3}",              # Bell g codes have three digit arguments
    "\\x07K",                    # Bell K codes have no args
    "\\x07R\\d{2}?",             # Bell R codes can have two digit or zero arguments
    "\\x07c.*$",                 # Bell c, in all cases, span the entire line
    "\\x07h\\d",                 # Bell h have one digit args
    "\\x07L",                    # Bell L have no args
    "\\x07b",                    # Bell b have no args
    "\\x07Q\\d{2}",              # Bell Q codes have two digit arguments
    "\\x07q\\d{2}",              # Bell q codes have two digit arguments
    "\\x07j",                    # Bell j have no args
    "\\x07D",                    # Bell D have no args
    "\\x07oi\\d",                # Bell oi have one digits args
    "\\x07e",                    # Bell e have no args
    "\\x07P",                    # Bell P have no args
    "\\x07l",                    # Bell l have no args
    "�.*\\x07N",                 # Bell N codes seem to pair with �
    "\\x07N.*�",                 # Bell N codes seem to pair with �
    "\\x07N"                     # Bell N codes seem to pair with �
  )

  combined_pattern <- str_c(patterns, collapse = "|")

  return (stringr::str_remove_all(line, combined_pattern))
}


# remove other wierd characters
# IMPORTANT: this needs to go
remove_other_characters <- function(line) {

  return (
    line %>%
      stringr::str_remove_all("\\\\\\d*\\\\") %>% # remove footnote references
      stringr::str_remove("[^[:print:]]") # other non-alphanumeric characters
  )
}

replace_currency_references <- function(line) {
  ##line <- "\aI02Over $70,000 but not over $125,000 \aD\aoi2$17,964.25, plus 36% of the excess over $70,000. Don't match 2.45. do match $1.3. don't match 12,560, do match $1.45 $2 million, $60 million, $15.5 million, $2.5 million $7.3 billion, $100 billion, $.7 billion $1.56 billion don't match 4.56 don't match 1.4 do $1.23 "

  ## just need to match $xx.x "million" and "billion" and insert underscore before alpha descriptor
  ## then match currency with $'s

  ## -- match $xx.xx (million|trillion) and put underscore before (million|trillion) --
  # cases from USC26 include: $2 million, $60 million, $15.56 million, $2.5 million
  # $7.3 billion, $100 billion, $.7 billion
  alpha_num_currency <- "(?<=\\$(\\d{0,3})(\\.)?(\\d{1,3})?)( )(?=(million|billion))"

  # match commas that come AFTER a $ and 1-3 digits (have to also allow for zero or more
  # sets of comma and three digits) and BEFORE 3 digits
  comma_in_currency <- "(?<=\\$(\\d{1,3}(\\,\\d{3}){0,4}))(\\,)(?=\\d{3})"

  # match decimals AFTER a $ (and possibly some numbers with commas) and BEFORE 1-2 digits
  # NOTE: commas will already be replaced with underscores, so checking for underscores to
  # separate thousands, millions, etc.
  decimal_in_currency <- "(?<=\\$(\\d{1,3}(\\_\\d{3}){0,4})?)(\\.)(?=\\d{1,2})"

  return (
    line %>%
      str_replace_all(alpha_num_currency, "_") %>%
      str_replace_all(comma_in_currency, "_") %>%
      str_replace_all(decimal_in_currency, "_")
  )

}

replace_percent_references <- function(line) {
  ## line <- "34.5% 15.5-percent 45.75 percent 2.75-percent 15-percent \aI02Over $70,000 but not over $125,000 \aD\aoi2$17,964.25, plus 36% of the excess over $70,000. 34.5 percent. 55 percentage"

  # Examples from USC26 include:
  # 39.6 percent
  # 15-percent
  # 36 percent
  # 3 percent
  # 200 percent

  ## NOTE: I found one case of "thirty-five percent", but figure such cases will be either 1)
  ## immaterial or 2) will handled by n-gram windows.
  decimal_percent <- "(?<=(\\d{0,3}))(\\.)(?=(\\d{1,2}%|\\d{0,2}[ -]percent))"
  alpha_num_percent <- "(?<=(\\d{0,3})(\\_\\d{1,2})?)([ -])(?=percent)"

  # TODO: Fix alpha_num_percent pattern to not match "7th such taxable year, 1/3 of the percentage"
  return (
    line %>%
      str_replace_all(decimal_percent, "_") %>%
      str_replace_all(alpha_num_percent, "_") %>%
      str_replace_all("%", "_percent")
  )

}


# this function encapsulates all text prep functions
# IMPORTANT: Careful! Order of function calls matters!
cleanup_text <- function(line) {
  return (
    line %>%
      replace_currency_references() %>%
      replace_percent_references() %>%
      # remove_bell_characters() %>%
      str_trim(side = "both") %>%
      remove_other_characters() # call last, removes non-alphanum(punct) characters
  )
}

new_text <- cleanup_text(text) %>% stri_remove_empty() %>%  str_c(collapse = " ")

### Sandbox
library(tidytext)

wordlist <- c(
  "section",
  "pub",
  str_c(1:9),
  "i",
  "ii",
  "iii",
  "iiii"
)
tax_stopwords <- tibble(word = wordlist)

tidy_text <- tibble(line = 1:length(new_text), text = new_text)
tidy_text <- tidy_text %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(tax_stopwords)

tidy_text %>%
  count(word, sort = TRUE) %>%
  print(n = 50)




#' PCC_URL <- "https://uscode.house.gov/download/releasepoints/us/pl/116/91/pcc_usc26@116-91.zip"
#' DATA_PATH <- "./R/sysdata.rda"
#'
#' ## Downloading/unzipping cross platform may be devilish details
#'
#' #' Download PCC Version of US Code 26 (Internal Revenue Code) from the Internet
#' #'
#' #' @param URL to PCC zip file
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' retrieve_pcc_data <- function(url) {
#'   temp <- tempfile()
#'   path <- fs::path_join(c(DATA_PATH, "usc26.pre"))
#'   utils::download.file(url, temp, mode = "wb")
#'   utils::unzip(zipfile = temp, exdir = DATA_PATH)
#'   unlink(temp)
#'   if (file.exists(path)) {
#'     return(path)
#'   } else {
#'     return(NULL)
#'   }
#'
#' }


unify_references <- function() {

}

