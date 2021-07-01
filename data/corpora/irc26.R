## code to prepare `irc26` dataset goes here
library(xml2)
library(stringr)
library(fs)

rds_dir <- "/data/rstudio/rds"
rds_path <- path(rds_dir, "irc26.rds")

temp_dir <- "/data/rstudio/temp"
temp_file_path <- path(temp_dir, "usc26.xml")

download_irc26 <- function() {

  irc26_url <- "https://uscode.house.gov/download/releasepoints/us/pl/116/138not113/xml_usc26@116-138not113.zip"
  tempfile <- path(tempdir(), "usc26xml.zip")
  download.file(irc26_url, tempfile)
  unzip(tempfile, exdir = temp_dir)

}


extract_irc26 <- function(removeNotes = TRUE, removeTOC = TRUE) {

  rds_dir <- "/data/rstudio/rds"
  rds_path <- path(rds_dir, "irc26.rds")
  
  temp_dir <- "/data/rstudio/temp"
  temp_file_path <- path(temp_dir, "usc26.xml")

  message("-----Extracting IRC26-----")
  
  # if the file has already been downloaded and processed, just load the RDS
  # and return the data
  if(file_exists(rds_path)) {
    message(str_glue("'{rds_path}' exists. Loading and returning."))
    irc26 <- readRDS(rds_path)
    return(invisible(irc26))
  }
  
  # only download if necessary
  if (!file_exists(temp_file_path)) {
    message("downloading usc26xml.zip...")
    download_irc26()
  }

  message("reading usc26.xml...")
  xml <- read_xml(temp_file_path)

  ## Some <section> nodes are contained inside <notes>
  ## -- begin <section> count = 2217 -- this includes all <section> nodes, some of
  ## which do not corresponding to actual code sections.
  ## -- after removing <sourceCredit> nodes, <section> count = 2217
  ## -- after removing <notes> nodes, <section> count = 2106
  ## -- after removing rest of <note> nodes, <section> count = 2106
  ##
  ## I'm guessing these sections insides <notes> do not correspond to actual
  ## code sections. Regardless, this is fine for now. We are painting broad
  ## strokes with our word embedding. Need to check if removing notes and toc
  ## makes a difference. Can fine tune later.
  if (removeNotes) {

    message("removing IRC notes...")

    ## Based on USLM User Guide section 7.7, the following nodes derive from
    ## <note>: <sourceCredit>, <statutoryNote>, <editorialNote>, and <changeNote>.
    ## Only found <sourceCredit> nodes in the IRC26.
    source_credits <- xml_find_all(xml, "//d1:sourceCredit", ns = xml_ns(xml))
    xml_remove(source_credits)
    rm(source_credits)


    ## remove all <notes> collections
    notes <- xml_find_all(xml, "//d1:notes", ns = xml_ns(xml))
    xml_remove(notes)
    rm(notes)

    ## some notes are note part of notes collections
    note <- xml_find_all(xml, "//d1:note", ns = xml_ns(xml))
    xml_remove(note)
    rm(note)

  }

  if (removeTOC) {

    message("removing IRC table of content items...")

    ## asserted that all <tocItem> nodes were inside <toc> nodes,
    ## so just removing <toc>
    toc <- xml_find_all(xml, "//d1:toc", ns = xml_ns(xml))
    xml_remove(toc)
    rm(toc)

  }

  ## The removal of notes and TOC items should have also removed <section> nodes
  ## that did not correspond to actual code sections (e.g., § 4411), so just
  ## querying for <section> tags and not worrying whether the <num> child has a
  ## value or not (see commented out code below)
  sections <- xml_find_all(xml, "//d1:section", ns = xml_ns(xml))

  ### retrieve all the section nodes - actual IRC section nodes
  ### Not all <section> tags correlated with actual IRC section nodes, hence
  ### I'm checking that the <num> node contains a non-empty value attribute
  # sections <-
  #   xml_find_all(xml, "//d1:section/d1:num[string(@value)]/..", ns = xml_ns(xml))

  # if we just use xml_text() to retrieve the text (i.e. remove xml tags), it does not put spaces between the text of the
  # different subnodes. So we have to select all the text nodes individually and then paste them together with a space.
  message("extracting IRC text...")
  extract_text_from_node <- function(node) {
    require(xml2)
    text_nodes <- xml_find_all(node, ".//text()")
    section_text <- str_c(xml_text(text_nodes), collapse = " ")
    section_text
  }

  irc26 <- sapply(sections, extract_text_from_node)
  # saveRDS(irc26, "data/cleaned_irc26.rds")
  message("saving irc26.rds...")
  
  saveRDS(irc26, rds_path, compress = FALSE)
  
  return(invisible(irc26))
}
