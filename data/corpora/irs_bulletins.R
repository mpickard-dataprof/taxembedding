library(fs)
library(stringi)
library(stringr)
library(rvest)
library(readr)
library(xml2)
library(purrr)
library(furrr)
library(dplyr)



get_links_from_page <- function(page_url) {
  message(str_glue("Scraping IRS Bulletin links from {page_url}..."))
  page_html <- read_html(page_url)
  urls <- page_html %>% html_nodes("tbody tr > td:first-of-type > a") %>% html_attr("href")
  return(urls)
}

get_page_urls <- function() {
  
  # scrape IRS Bulletins - available in HTML back to 2003 (that's sufficient)
  home_url <- "https://www.irs.gov/irb"
  base_page_url <- "https://www.irs.gov/irb?page="
  
  # first find max page number
  page_html <- read_html(home_url)
  max_page <- page_html %>%
    html_node("li.pager__item--last a") %>%
    html_attr("href") %>%
    str_match("(?<=page=)[:digit:]{1,3}") %>%
    parse_number()
  
  page_urls <- str_c(base_page_url, 1:max_page)
  
  page_urls <- unlist(map(page_urls, get_links_from_page))
  
  return(page_urls)
}

scrape_page_html <- function(url) {
  message(str_glue("Scraping text from {url}..."))
  pagetext <- read_html(url) %>% 
    html_nodes("div.book") %>% 
    html_text()
  return(pagetext)
}

extract_bulletins <- function() {
  
  rds_dir <- "/data/rstudio/rds"
  rds_path <- path(rds_dir, "bulletins.rds")
  
  message("-----Extracting IRS Bulletins-----")
  
  # if the file has already been downloaded and processed, just load the RDS
  # and return the data
  if(file_exists(rds_path)) {
    message(str_glue("'{rds_path}' exists. Loading and returning."))
    bulletins <- readRDS(rds_path)
    return(invisible(bulletins))
  }
  
  bulletin_urls <- get_page_urls()
  
  plan(multiprocess, workers = 10)
  text <- unlist(future_map(bulletin_urls, scrape_page_html))
  
  saveRDS(text, rds_path, compress = FALSE)
  
  return(invisible(text))
  
}


