## code to prepare `ecfr26` dataset goes here
library(xml2)
library(stringr)
library(fs)


download_ecfr26 <- function() {

  ecfr26_url <- "https://www.govinfo.gov/bulkdata/ECFR/title-26/ECFR-title26.xml"
  download.file(ecfr26_url, temp_file_path)

}

extract_ecfr26 <- function(removeNotes = TRUE, removeTOC = TRUE) {

  rds_dir <- "/data/rstudio/rds"
  rds_path <- path(rds_dir, "ecfr26.rds")
  
  temp_dir <- "/data/rstudio/temp"
  temp_file_path <- file.path(temp_dir, "ecfr26.xml")

  message("-----Extracting eCFR26-----")

  # if the file has already been downloaded and processed, just load the RDS
  # and return the data
  if(file_exists(rds_path)) {
    message(str_glue("'{rds_path}' exists. Loading and returning."))
    ecfr26 <- readRDS(rds_path)
    return(invisible(ecfr26))
  }
  
  # only download if necessary
  if (!file_exists(temp_file_path)) {
    message("downloading ecfr26.xml...")
    download_ecfr26()
  }

  message("reading ecfr26.xml...")
  xml <- read_xml(temp_file_path)

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
  message('extracting nodes from ecfr26...')
  sections <- xml_find_all(xml, "//DIV8")

  # if we just use xml_text() to retrieve the text (i.e. remove xml tags), it does not put spaces between the text of the
  # different subnodes. So we have to select all the text nodes individually and then paste them together with a space.
  message('extracting text from ecfr26...')
  extract_text_from_node <- function(node) {
    require(xml2)
    text_nodes <- xml_find_all(node, ".//text()")
    section_text <- str_c(xml_text(text_nodes), collapse = " ")
    section_text
  }

  ecfr26 <- sapply(sections, extract_text_from_node)
  # saveRDS(ecfr26, "data/cleaned_ecfr26.rds")
  message('saving ecfr26.rds...')
  
  saveRDS(ecfr26, rds_path, compress = FALSE)
  
  return(invisible(ecfr26))

}
