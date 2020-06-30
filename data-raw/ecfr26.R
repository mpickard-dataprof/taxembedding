## code to prepare `ecfr26` dataset goes here
library(xml2)
library(stringr)
library(fs)

download_ecfr26 <- function() {

  ecfr26_url <-
    "https://www.govinfo.gov/bulkdata/ECFR/title-26/ECFR-title26.xml"
  file_path <- path("data-raw", "ecfr26.xml")
  download.file(ecfr26_url, file_path)

}

extract_ecfr26 <- function(removeNotes = TRUE, removeTOC = TRUE, force_refresh = FALSE) {

  ### NOTE: I believe 'use_data()' sets up lazy loading of these data sets.
  ### So, no need to explicitly load them. They are saved by 'use_data()'
  ### as .rda files anyway
  ###
  # cleaned_file_path <- file.path("data", "cleaned_ecfr26.rds")
  #
  # if (file_exists(cleaned_file_path) && !force_refresh) {
  #   ecfr26 <- readRDS(cleaned_file_path)
  #   invisible(ecfr26)
  # }

  raw_file_path <- file.path("data-raw", "ecfr26.xml")

  if (!file_exists(raw_file_path)) {
    download_ecfr26()
  }

  xml <- read_xml(raw_file_path)

  ## eCFR has 22 volumes.
  ## Each volume has a <CFRTOC> which is short and repetitive, so remove it.
  ## There does not seem to be any substantial text until we get down to the
  ## section level (which corresponds to IRC26), which is denoted with
  ## <DIV8...TYPE="SECTION">. Every <DIV8> tag has TYPE = 'SECTION',
  ## so just pulling all <DIV8> nodes.

  ## Unlike USC26, the TOC items are outside the sections, so no need to remove
  ## them. There are some note like source references at the bottom of some (all?)
  ## sections, but there's no clean way to remove them without removing other
  ## parts that should be left in.

  ## Also, the eCFR XML document does not use namespaces

  sections <- xml_find_all(xml, "//DIV8")

  # if we just use xml_text() to retrieve the text (i.e. remove xml tags), it does not put spaces between the text of the
  # different subnodes. So we have to select all the text nodes individually and then paste them together with a space.
  extract_text_from_node <- function(node) {
    require(xml2)
    text_nodes <- xml_find_all(node, ".//text()")
    section_text <- str_c(xml_text(text_nodes), collapse = " ")
    section_text
  }

  ecfr26 <- sapply(sections, extract_text_from_node)
  # saveRDS(ecfr26, "data/cleaned_ecfr26.rds")
  usethis::use_data(ecfr26, overwrite = TRUE)
  invisible(ecfr26)

}

