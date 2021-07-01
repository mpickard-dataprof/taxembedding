library(fs)
library(stringi)
library(stringr)
library(rvest)
library(readr)
library(xml2)
library(purrr)
library(furrr)
library(dplyr)
library(textcat)


# scrape IRS Bulletins - available in HTML back to 2003

get_page_urls <- function() {
  
  home_url <- "https://www.irs.gov/publications"
  message("Scraping IRS Publication URLS....")
  
  # first find max page number
  page_html <- read_html(home_url)
  urls <- page_html %>% html_nodes("tbody > tr > td:nth-child(2) > a") %>% html_attr("href")
  return(urls)
}

scrape_page_html <- function(url) {
  
  message(str_glue("Scraping text from {url}..."))
  tryCatch(
    {
      pagetext <- read_html(url) %>%
        html_nodes("div.book") %>%
        html_text()
      
      # only return English documents (not Spanish)
      if(str_detect(textcat(pagetext), "english")) {
        return(pagetext)
      }
    },
    error = function(c) 
      {
        message(str_glue("Error reading {url}. Returning empty string..."))
        return("")
      }
    )
}

extract_publications <- function() {
  
  rds_dir <- "/data/rstudio/rds"
  rds_path <- path(rds_dir, "publications.rds")
  
  message("-----Extracting IRS Publications-----")
  
  # if the file has already been downloaded and processed, just load the RDS
  # and return the data
  if(file_exists(rds_path)) {
    message(str_glue("'{rds_path}' exists. Loading and returning."))
    publications <- readRDS(rds_path)
    return(invisible(publications))
  }
  
  publication_urls <- get_page_urls()
  
  plan(multiprocess, workers = 10)
  text <- unlist(future_map(publication_urls, scrape_page_html))
  
  saveRDS(text, rds_path, compress = FALSE)
  
  return(invisible(text))
  
}


