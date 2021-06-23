library(fs)
library(stringi)
library(rvest)
library(purrr)
library(furrr)
library(tidyr)
library(httr)
library(jsonlite)
library(dplyr)
library(stringr)

# used to remove cases from courts that overlap with courtlistener
courtlistener_courts <- c("or-tc", "tc", "bta-1", "mont-tc")
`%notin%` <-  Negate(`%in%`)

temp_rds_path <- path(temp_dir, "case-law-download-contains-tax.rds")
caselaw_access_token <- "ea803ecf0ca18cca2fe0f8cc536740feb7e9cc6b"

## A comparison between the CourtListener.com and case.law repositories.
## CourtListener had larger data sets for "bta", "tax", and "monttc" courts. 

download_caselaw <- function(court, url) {
  
  temp_dir <- "/data/rstudio/temp"
  sleep_time <- 60
  num_times_slept <- 0
  
  if (!file_exists(path(temp_rds_path))) {
    # NOTE: 10,000 results didn't complete, 1,000 did. 
    # settling on 2,000 at a time.
    search_url <-
      "https://api.case.law/v1/cases/?search=tax&full_case=true&page_size=2000"
    
    cases_df <- data.frame()
    
    while (!is.null(search_url)) {
      cases_json <- content(GET(search_url,
                                add_headers(
                                  Authorization = str_glue('Token {caselaw_access_token}')
                                )),
                            "text", encoding = "utf8")
      
      if (validate(cases_json)) {
        message(str_glue("Sucessfully downloaded {search_url}"))
        
        cases <-
          fromJSON(cases_json,
                   flatten = TRUE,
                   simplifyDataFrame = TRUE)
        
        # NOTE: not worrying about removing cases that overlap with courts from CourtListener. 
        # Working on the assumption that there's not a perfect overlap and that significant case duplication
        # is insignificant for training the embedding.
        results <- cases$results %>%
          filter(court.slug %notin% courtlistener_courts) %>%
          select(
            name,
            url,
            court.id,
            court.name,
            court.slug,
            docket_number,
            jurisdiction.id,
            jurisdiction.name_long,
            jurisdiction.name,
            jurisdiction.slug,
            reporter.full_name,
            analysis.word_count,
            analysis.ocr_confidence,
            casebody.data.head_matter,
            casebody.data.opinions,
            casebody.status
          )
        
        cases_df <- cases_df %>% bind_rows(results)
        message(str_glue("Added new search results to data.frame...dataframe has {nrow(cases_df)}..."))
        
        search_url <- cases$`next`
      } else {
        
        if (num_times_slept > 3) {
          stop(str_glue("Stopping. Tried to download three times: {case_url}"))
        }
        
        print(str_glue("{case_url} is not valid JSON."))
        Sys.sleep(sleep_time)
        sleep_time <- sleep_time + 60
        num_times_slept <- num_times_slept + 1
        
      }
    }
    
    # save the RDS (temporary file), so we don't have to download files from case.law in the future
    saveRDS(cases_df, temp_rds_path, compress = FALSE)
    message(str_glue("Sucessfully saved temporary RDS download from case.law: {temp_rds_path}!"))
    
  } else {
    message(str_glue("{temp_rds_path} already exists. Not downloading."))
    return(invisible(NULL))
  }
  
  return(invisible(cases_df))
  
}


extract_case_opinion <- function() {
  
  # load download cases
  # WARNING: this takes a while to load. So only load it, if it's not already in memory.
  if (!exists("cases_df")) {
    message("case law RDS file not loaded. Loading...")
    cases_df <-
      readRDS(temp_rds_path)
  }
  
  message("extracting opinions from cases...")
  opinion_text <-
    cases_df %>% unnest(casebody.data.opinions) %>% pull(text)
  
  return(invisible(opinion_text))
}

extract_caselaw <- function() {

  rds_dir <- "/data/rstudio/rds"
  rds_path <- path(rds_dir, "caselaw.rds")
  
  message("-----Extracting case.law cases-----")
  
  # if the file has already been downloaded and processed, just load the RDS
  # and return the data
  if(file_exists(rds_path)) {
    message(str_glue("'{rds_path}' exists. Loading and returning."))
    caselaw <- readRDS(rds_path)
    return(invisible(caselaw))
  }
  
  # caselaw.rds does not exist, so create it
  download_caselaw()
  caselaw <- extract_case_opinion()
  
  # this is the final version, ready for embedding processing
  message("saving caselaw.rds...")
  saveRDS(caselaw$text, rds_path, compress = FALSE)
  
  return(invisible(caselaw))

}
