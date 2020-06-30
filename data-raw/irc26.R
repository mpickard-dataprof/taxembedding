## code to prepare `irc26` dataset goes here
library(xml2)
library(stringr)
library(fs)

download_irc26 <- function() {

  ## TODO: to download latest version, could scrape all URLs on "https://uscode.house.gov/download/download.shtml" filter
  ## out URL with "xml_usc26" in it and use that as the "irc26_url" url path.

  irc26_url <-
    "https://uscode.house.gov/download/releasepoints/us/pl/116/138not113/xml_usc26@116-138not113.zip"
  file_path <- path(tempdir(), "usc26xml.zip")
  download.file(irc26_url, file_path)
  unzip(file_path, exdir = "data-raw")

}


extract_irc26 <- function(removeNotes = TRUE, removeTOC = TRUE) {

  ### NOTE: I believe 'use_data()' sets up lazy loading of these data sets.
  ### So, no need to explicitly load them. They are saved by 'use_data()'
  ### as .rda files anyway
  ###
  # cleaned_file_path <- file.path("data", "cleaned_irc26.rds")
  #
  # if (file_exists(cleaned_file_path) && !force_refresh) {
  #   irc26 <- readRDS(cleaned_file_path)
  #   invisible(irc26)
  # }

  raw_file_path <- file.path("data-raw", "usc26.xml")

  if (!file_exists(raw_file_path)) {
    message("downloading usc26xml.zip...")
    download_irc26()
  }

  message("reading usc26.xml...")
  xml <- read_xml(raw_file_path)

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
  message("saving irc26.rda...")
  usethis::use_data(irc26, overwrite = TRUE)
  invisible(irc26)

}


