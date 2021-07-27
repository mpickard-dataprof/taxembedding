library(fs)
library(stringi)
library(xml2)
library(rvest)
library(purrr)
library(furrr)
library(tibble)


## A comparison between the CourtListener.com and case.law repositories.
## CourtListener had larger data sets for "bta", "tax", and "monttc" courts. 
clist_base_url <- "https://www.courtlistener.com/api/bulk-data/opinions/{court}.tar.gz"
clist_courts <- c("bta", "tax", "monttc", "ortc")
clist_urls <- str_replace(clist_base_url, "\\{court\\}", clist_courts)
clist_courts_df <- tibble(court = clist_courts , url = clist_urls)
temp_dir <- "/data/rstudio/temp"

download_courtlistener <- function(court, url) {

  #TODO: add tryCatch code for download and untar errors
  download_file_path <- path(temp_dir, path_file(url))
  
  if(!file_exists(download_file_path)) {
    download.file(url, download_file_path)
  }
  
  court_dir <- path(temp_dir, "courtlistener", court)
  if (!dir_exists(court_dir)) {
    dir_create(court_dir)
  } 
  
  untar(download_file_path, exdir = court_dir)
}

download_courtlistener_cases <- function() {
  require(purrr)
  cl_cases_df <- pmap(clist_courts_df, download_courtlistener)
  return(invisible(cl_cases_df))
}


## Helper function: check which courtlistener case fields have text in them
is.textavailable <- function(object) {
  if(!is.null(object)) {
    if(!stri_isempty(object))
      return(TRUE)
  }
  return(FALSE)
}

extract_text_from_case_inner <- function(json_path) {
  json <- fromJSON(json_path)
  case_text <- ""
  
  if (is.textavailable(json$plain_text)) {
    case_text <- str_c(case_text, json$plain_text)
  }
  
  if (is.textavailable(json$html)) {
    text <- html_text(read_html(json$html))
    case_text <- str_c(case_text, text)
  }
  
  if (is.textavailable(json$html_lawbox)) {
    text <- html_text(read_html(json$html_lawbox))
    case_text <- str_c(case_text, text)
  }
  
  if (is.textavailable(json$html_columbia)) {
    text <- html_text(read_html(json$html_columbia))
    case_text <- str_c(case_text, text)
  }
  
  if (is.textavailable(json$xml_harvard)) {
    x <- read_xml(json$xml_harvard)
    nodes <- xml_find_all(x, "//text()")
    text <- str_c(as_list(nodes), collapse = " ")
    case_text <- str_c(case_text, text)
  }
  
  # NOTE: In the future, courtlistener.com will place the final web page html in the "html_with_citations" section, so you could just grab it from there regardless of its original source.  (https://www.courtlistener.com/api/rest-info/#opinion-endpoint)

  return(invisible(case_text))
}

extract_text_from_case <- function(json_path) {
  tryCatch(
    text <- extract_text_from_case_inner(json_path),
    error = function(cond) {
      message(str_glue("Error extracting text from {json_path}"))
    }
  )
}

extract_courtlistener <- function() {

  rds_dir <- "/data/rstudio/rds"
  rds_path <- path(rds_dir, "courtlistener.rds")
  
  message("-----Extracting CourtListener cases-----")
  
  # if the file has already been downloaded and processed, just load the RDS
  # and return the data
  if(file_exists(rds_path)) {
    message(str_glue("'{rds_path}' exists. Loading and returning."))
    courtlistener <- readRDS(rds_path)
    return(invisible(courtlistener))
  }
  
  json_list <- fs::dir_ls(path(temp_dir, clist_courts))
  
  download_courtlistener_cases()
  
  plan(multiprocess, workers = 10)
  courtlistener <- as.vector(furrr::future_map_chr(json_list, extract_text_from_case))

  message("saving courtlistener.rds...")
  saveRDS(courtlistener, rds_path, compress = FALSE)
  
  return(invisible(courtlistener))

}
