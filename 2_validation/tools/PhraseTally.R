########################################################################################
#            |                                                                         #
# FILE:      | PhraseTally.R                                                           #
#------------|-------------------------------------------------------------------------#
# USE:       | This script counts the occurences of phrases for certain word_pairs and #
#            | analogies within a text corpus                                          #
#------------|-------------------------------------------------------------------------#
# AUTHOR(S): | Alexander Wold (Z1817662)                                               #
#------------|-------------------------------------------------------------------------#
# DATE:      | 10/12/2020                                                              #
#            |                                                                         #
########################################################################################


##############################################################################
#          |                                                                 #
# SECTION: | OVERHEAD CONSTANTS                                              #
#----------|-----------------------------------------------------------------#
# USE:     | here, the user can define certain variables that change the     #
#          | behavior of the script                                          #
#          |                                                                 #
##############################################################################
RDS_FILE_LOCATION <- "/data/rstudio/corpi"
INPUT_FILE_PATH <- "~/Projects/taxembed/2_validation/ngrams_master.csv"
OUTPUT_FILE_PATH <- "~/Projects/taxembed/2_valdiation"
ANALOGY_SWITCH <- TRUE
INCLUDE_COUNTS <- FALSE
CSV_SWITCH <- FALSE
CUTOFF <- 10


##############################################################################
#          |                                                                 #
# SECTION: | PACKAGES                                                        #
#----------|-----------------------------------------------------------------#
# USE:     | loads the required packages                                     #
#          |                                                                 #
##############################################################################
library(stringr)
library(tidyverse)
library(parallel)
library(multidplyr)
library(fs)
library(rapportools)


##############################################################################
#          |                                                                 #
# SECTION: | FUNCTIONS                                                       #
#----------|-----------------------------------------------------------------#
# USE:     | provides fundamental functionality for the script               #
#          |                                                                 #
##############################################################################
################################################################
#           |                                                  #
# FUNCTION: | grab_scores                                      #
#-----------|--------------------------------------------------#
# USE:      | this function grabs the phrases, scores, and     #
#           | analogies from the specified files               #
#           |                                                  #
################################################################
grab_scores <- function(file_path = NULL, is_analogy = FALSE)
{
  # create an empty frame to hold the phrase data
  d_frame <- NULL

  if (is_analogy == FALSE)
  {
    # pull the wordsim information from a file and put it into a dataframe
    if (CSV_SWITCH == TRUE) {
      d_frame <- read.csv(file_path, header = FALSE, stringsAsFactors = FALSE)
    } else {
      d_frame <- read.table(file_path, header = FALSE, stringsAsFactors = FALSE)
    }

    # check if scores were passed in, if not, create a score column with NA values
    if (ncol(d_frame) == 2) {
      d_frame <- cbind(drame, rep(NA, length(d_frame[1])))
      names(d_frame) <- c("phrase1", "phrase2", "score")
    } else if (ncol(d_frame) == 3) {
      names(d_frame) <- c("phrase1", "phrase2", "score")
    }
  }

  else
  {
    # create a variable to hold the read in text data
    text_object <- NULL
    
    # read in the data - account for a csv of analogy data
    if (CSV_SWITCH == FALSE) {
      text_object <- readLines(INPUT_FILE_PATH)
    } else {
      temppath <- paste0(INPUT_FILE_PATH, "_temp")
      analogy_csv_frame <- read.csv(INPUT_FILE_PATH, header = FALSE, stringsAsFactors = FALSE)
      lines <- unlist(apply(analogy_csv_frame, MARGIN = 1, paste, collapse = " "))
      writeLines(text = lines, con = temppath)
      text_object <- readLines(temppath)
      file.remove(temppath)
    }
    
    # clean the analogy data (remove section names - they won't be read into the data frame)
    strippedpath <- paste0(INPUT_FILE_PATH, "_sections_stripped")
    text_object <- str_remove_all(string = text_object, pattern = regex(multiline = TRUE, pattern = "^.*:.*$"))
    writeLines(text = text_object, con = strippedpath)

    # read the data into a data frame
    d_frame <- read.table(strippedpath, header = FALSE, stringsAsFactors = FALSE,
                              col.names = c("particular1_phrase1", "particular1_phrase2",
                                            "particular2_phrase1", "particular2_phrase2"))
  }

  d_frame
}

################################################################
#           |                                                  #
# FUNCTION: | word_totals                                      #
#-----------|--------------------------------------------------#
# USE:      | this function counts the number of times a       #
#           | phrase appears in the irc and ecfr - it is       #
#           | utilized by the count_phrases and                #
#           | count_analogies function                         #
#           |                                                  #
################################################################
word_totals <- function(phrase)
{
  phrase <- str_replace_all(string = phrase, pattern = "\\(", replacement = "\\\\(")
  phrase <- str_replace_all(string = phrase, pattern = "\\)", replacement = "\\\\)")
  phrase <- str_replace_all(string = phrase, pattern = "\\?", replacement = "\\\\?")
  
  search_pattern <- paste0("(?i)", "\\b", phrase, "\\b")
  sum(str_count(string = searchable_text, pattern = search_pattern))
}

################################################################
#           |                                                  #
# FUNCTION: | get_unique_strings                               #
#-----------|--------------------------------------------------#
# USE:      | this function returns a list of unique strings   #
#           | from read from an analogies document             #
#           |                                                  #
################################################################
get_unique_strings <- function(df)
{
  # first, get the unique strings from each row in a passed data frame
  # do not conserve names and flatten the list, then get the unique strings from the master list
  unique_df <- unique(unlist(apply(X = df, MARGIN = 2, FUN = unique), use.names = FALSE))
  unique_df
}

################################################################
#           |                                                  #
# FUNCTION: | get_count                                        #
#-----------|--------------------------------------------------#
# USE:      | this function looks up a string from the table   #
#           | of unique strings and returns its count value    #
#           |                                                  #
################################################################
get_count <- function(phrase, u_df)
{
  count <- u_df[u_df$phrase == phrase, c("phrase_counts")]
  count
}

################################################################
#           |                                                  #
# FUNCTION: | get_count_from_array                             #
#-----------|--------------------------------------------------#
# USE:      | this function iterates over a list of analogy    #
#           | phrases and calls get_count on each element      #
#           |                                                  #
################################################################
get_count_from_array <- function(phrase_array, u_df)
{
  count_array <- unlist(lapply(X = phrase_array, FUN = get_count, u_df = u_df), use.names = FALSE)
  count_array
}

################################################################
#           |                                                  #
# FUNCTION: | analogy_count                                    #
#-----------|--------------------------------------------------#
# USE:      | using a data frame of unique phrase counts, this #
#           | function appends count data for each phrase of   #
#           | a particular analogy                             #
#           |                                                  #
################################################################
analogy_count <- function(unique_count_df, analogy_df)
{
  # for each column in the analogy data frame, call the get_count_from_array function
  analogy_count_df <- data.frame(apply(X = analogy_df, MARGIN = 2, FUN = get_count_from_array, u_df = unique_count_df))

  # change the names of the data frame
  names(analogy_count_df) <- c("particular1_phrase1_count", "particular1_phrase2_count", "particular2_phrase1_count", "particular2_phrase2_count")

  # bind the columns of both the count data and the analogy data
  combined_df <- cbind(analogy_df, analogy_count_df)

  # reorder the rows
  combined_df <- combined_df[ , c("particular1_phrase1", "particular1_phrase1_count",
                                  "particular1_phrase2", "particular1_phrase2_count",
                                  "particular2_phrase1", "particular2_phrase1_count",
                                  "particular2_phrase2", "particular2_phrase2_count")]


  combined_df
}

################################################################
#           |                                                  #
# FUNCTION: | count_phrases                                    #
#-----------|--------------------------------------------------#
# USE:      | this function uses parallel programming to count #
#           | the number of times a phrase appears in the irc  #
#           | and ecfr - it takes a dataframe of the form:     #
#           | phrase1, phrase2, similarity, relatedness        #
#           |                                                  #
################################################################
count_phrases <- function(score_df)
{
  # count the number of times the first phrase in each phrase pair appears in the searchable text

  # get the number of available cores
  c1 <- detectCores()

  # group the cores and create empty count columns
  group <- rep_len(1:c1, length.out = nrow(score_df))
  phrase1_counts <- rep_len(x = 0, length.out = nrow(score_df))
  phrase2_counts <- rep_len(x = 0, length.out = nrow(score_df))

  # create new df with group assignments
  score_df <- data.frame(group = group, score_df)

  # create the clusters
  cluster <- new_cluster(c1)

  # set up the clusters
  cluster %>%
    # assign libraries
    cluster_library("stringr") %>%
    # assign values
    cluster_assign("searchable_text" = searchable_text) %>%
    cluster_assign("word_totals" = word_totals)

  # partition the dataframe by core group
  core_group <- score_df %>% group_by(group) %>% partition(cluster)

  # mutate the columns of
  count_df <- core_group %>%
    mutate(
      phrase1_counts = unlist(lapply(X = phrase1, FUN = word_totals)),
      phrase2_counts = unlist(lapply(X = phrase2, FUN = word_totals))
    ) %>%
    collect()

  # get rid of the groups column
  count_df$group <- c()

  # reorder the columns for visual sense
  all_names <- colnames(count_df)
  non_score_names <- c("phrase1", "phrase1_counts", "phrase2", "phrase2_counts")
  score_names <- setdiff(all_names, non_score_names)

  count_df <- count_df[ , c(non_score_names, score_names)]

  # return the data frame
  count_df
}

################################################################
#           |                                                  #
# FUNCTION: | count_analogies                                  #
#-----------|--------------------------------------------------#
# USE:      | this function works much like the count phrases  #
#           | function: it uses parallel programming to count  #
#           | the number of times each phrase appears in the   #
#           | irc and ecfr                                     #
#           |                                                  #
################################################################
count_analogies <- function(analogy_df)
{
  unique_df <- data.frame(get_unique_strings(analogy_df), stringsAsFactors = FALSE)

  # get the number of available cores
  c1 <- detectCores()

  # group the cores and create empty count columns
  group <- rep_len(1:c1, length.out = nrow(unique_df))
  phrase_counts <- rep_len(x = 0, length.out = nrow(unique_df))

  # create new df with group assignments
  unique_df <- data.frame(group = group, unique_df)
  names(unique_df) <- c("group", "phrase")

  # create the clusters
  cluster <- new_cluster(c1)

  # set up the clusters
  cluster %>%
    # assign libraries
    cluster_library("stringr") %>%
    # assign values
    cluster_assign("searchable_text" = searchable_text) %>%
    cluster_assign("word_totals" = word_totals)

  # partition the dataframe by core group
  core_group <- unique_df %>% group_by(group) %>% partition(cluster)

  # mutate the columns of
  count_df <- core_group %>%
    mutate(
      phrase_counts = unlist(lapply(X = phrase, FUN = word_totals))
    ) %>%
    collect()

  # get rid of the groups column
  count_df$group <- c()
  count_df <- data.frame(count_df)

  # build the phrase counts for each particular of an analogy
  count_df <- analogy_count(count_df, analogy_df)

  count_df
}

################################################################
#           |                                                  #
# FUNCTION: | cos_sim                                          #
#-----------|--------------------------------------------------#
# USE:      | this function converts a range to another range  #
#           | the default range is -1 to 1 (cosine similarity) #
#           |                                                  #
################################################################
cos_sim <- function(score_to_convert, oldmin = 0, oldmax = 10, newmin = -1, newmax = 1)
{
  oldrange <- oldmax - oldmin
  newrange <- newmax - newmin

  score_converted <- (((score_to_convert - oldmin) * newrange) / oldrange) + newmin
  score_converted
}


################################################################
#           |                                                  #
# FUNCTION: | sort_phrases                                     #
#-----------|--------------------------------------------------#
# USE:      | this function takes phrase count information and #
#           | sorts it based on user criteria.                 #
#           | it takes data in the form of:                    #
#           | phrase1, phrase1_counts, phrase2,                #
#           | phrase2_counts, similarity, relatedness          #
#           |                                                  #
################################################################
sort_phrases <- function(df, cutoff = 300, allMustQualify = TRUE, sortcol = "phrase1_counts", ascending = TRUE,
                         convert = FALSE, ...)
{
  # delete only the rows where ONE OR BOTH of the phrase counts don't meet the cutoff
  # (keep a row only if both phrase counts remain above or at the cutoff count)
  if (allMustQualify == TRUE)
  {
    df <- df[df$phrase1_counts >= cutoff & df$phrase2_counts >= cutoff, ]
  }

  # delete only the rows where NEITHER of the phrase counts meets the cutoff
  # (keep a row if at least one of the phrase counts remains above or at the cutoff count)
  else
  {
    df <- df[df$phrase1_counts >= cutoff | df$phrase2_counts >= cutoff, ]
  }

  # sort the data frame according to the user specifications
  if (ascending == TRUE) df <- df %>% arrange(get(sortcol)) else df <- df %>% arrange(desc(get(sortcol)))

  # if the user wants the similarity scores convert to a specific range, convert the scores
  if (convert == TRUE)
  {
    # get a list of column names that refer to the score information
    all_names <- colnames(count_results)
    non_score_names <- c("phrase1", "phrase1_counts", "phrase2", "phrase2_counts")
    score_names <- setdiff(all_names, non_score_names)

    df[score_names] <- df %>% select(score_names) %>% cos_sim(...)
  }

  # return the modified data frame
  df
}

################################################################
#           |                                                  #
# FUNCTION: | sort_analogies                                   #
#-----------|--------------------------------------------------#
# USE:      | this function takes a data frame with analogy    #
#           | count data and filters them according to user    #
#           | specifications                                   #
#           |                                                  #
################################################################
sort_analogies <- function(df, cutoff = 300, allMustQualify = TRUE, sortcol = "particular1_phrase1", ascending = TRUE, ...)
{
  # delete only the rows where ONE OR BOTH of the phrase counts don't meet the cutoff
  # (keep a row only if both phrase counts remain above or at the cutoff count)
  if (allMustQualify == TRUE)
  {
    df <- df[(df$particular1_phrase1_count >= cutoff) & (df$particular1_phrase2_count >= cutoff) & (df$particular2_phrase1_count >= cutoff) & (df$particular2_phrase2_count >= cutoff), ]
  }

  # delete only the rows where NONE of the phrase counts meets the cutoff
  # (keep a row if at least one of the phrase counts remains above or at the cutoff count)
  else
  {
    df <- df[(df$particular1_phrase1_count >= cutoff) | (df$particular1_phrase2_count >= cutoff) | (df$particular2_phrase1_count >= cutoff) | (df$particular2_phrase2_count >= cutoff), ]
  }

  # sort the data frame according to the user specifications
  if (ascending == TRUE) df <- df %>% arrange(get(sortcol)) else df <- df %>% arrange(desc(get(sortcol)))

  # return the modified data frame
  df
}



##############################################################################
#          |                                                                 #
# SECTION: | DRIVER                                                          #
#----------|-----------------------------------------------------------------#
# USE:     | uses the functions defined above to count the occurences of     #
#          | certain phrases within a provided, processed text corpus        #
#          |                                                                 #
##############################################################################
searchable_text <- readRDS(file = RDS_FILE_LOCATION)

# get the initial data frame of phrases to count
if (ANALOGY_SWITCH == FALSE) {
  phrase_frame <- grab_scores(file_path = INPUT_FILE_PATH, is_analogy = ANALOGY_SWITCH)
} else if (ANALOGY_SWITCH == TRUE) {
  phrase_frame <- grab_scores(file_path = INPUT_FILE_PATH, is_analogy = ANALOGY_SWITCH)
}


# print the dataframe
print(phrase_frame)

count_results <- NULL

# count the phrases or analogies
if (ANALOGY_SWITCH == FALSE)
{
  count_results <- count_phrases(phrase_frame)
} else if (ANALOGY_SWITCH == TRUE) {
  count_results <- count_analogies(phrase_frame)
}


print(count_results)


# create an empty variable to hold the sorted data frame
sorted_df <- NULL

# sort phrases or analogies
if (ANALOGY_SWITCH == FALSE)
{
  sorted_df <- sort_phrases(df = count_results, cutoff = CUTOFF, bothMustQualify = TRUE,
                            sortcol = "phrase1_counts", ascending = TRUE, convert = FALSE)
  
  if (INCLUDE_COUNTS == FALSE) {
    sorted_df <- sorted_df[, c("phrase1", "phrase2", "score")]
  }

  write.table(sorted_df, OUTPUT_FILE_PATH, row.names = FALSE, col.names = FALSE, quote = FALSE,
              sep = "\t")
  
} else {
  sorted_df <- sort_analogies(df = count_results, cutoff = CUTOFF, bothMustQualify = TRUE,
                              sortcol = "particular1_phrase1_count", ascending = TRUE)
  
  if (INCLUDE_COUNTS == FALSE) {
    sorted_df <- sorted_df[, c("particular1_phrase1", "particular1_phrase2",
                               "particular2_phrase1", "particular2_phrase2")]
  }
  
  section_title <- paste0(": >", toString(CUTOFF))
  writeLines(text = section_title, con = OUTPUT_FILE_PATH)
  write.table(sorted_df, OUTPUT_FILE_PATH, row.names = FALSE, col.names = FALSE,
              append = TRUE, quote = FALSE, sep = " ")
}


print(sorted_df)