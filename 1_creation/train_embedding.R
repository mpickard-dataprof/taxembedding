Sys.setenv(RETICULATE_PYTHON='/etc/anaconda/envs/embed_creation/bin/python')
library(reticulate)
library(dplyr)
library(fs)
library(readr)
library(purrr)
library(tidyr)

source("1_creation/process_corpus.R")

## PATHS FOR OUTPUTS
EMBED_PATH = '/data/rstudio/embeddings/sw_refs_ngrams'
PREP_CORPI_PATH = '/data/rstudio/corpi/prepared/sw_refs_ngrams'
TOK_CORPI_PATH = '/data/rstudio/corpi/tokenized/sw_refs_ngrams'

generateEmbeddings <- function() {

  # num_cores <- availableCores() / 2
  num_cores <- 12L

  ## check if 'corpi' and 'embeddings' directories exists
  ## if not, create them
  if (!dir_exists(PREP_CORPI_PATH)) {
    cat("'corpi' directory does not exist. Creating it...\n")
    message(str_glue("---Preparing corpus---"))
    dir_create(PREP_CORPI_PATH)
  }

  if (!dir_exists(EMBED_PATH)) {
    cat("'embeddings' directory does not exist. Creating it...\n")
    dir_create(EMBED_PATH)
  }

  ## NOTE [7/7/2021]: Based on initial analysis and knowledge of embeddings,
  ## I finalized the corpus parameters as follows.
  
  #####################################################################
  ################# Iteration #1 hyperparameters ######################
  #####################################################################
  
  ## NOTE [7/7/2021]: Based on initial analysis and knowledge of embeddings,
  ## I finalized the training parameters as follows. I selected these based on initial analysis of embedding results (see
  ## top 100 embedding analysis, and statistical comparison of similarity and 
  ## analogy tests).
  
  ## corpus parameters ##
  remove_stopwords <- c(FALSE) # swords important to some ngrams
  preserve_code_references <- c(TRUE) # these have meanings
  preserve_ngrams <- c(TRUE) # have meanings
  
  ## embedding parameters ##
  type <- c("word2vec", "fasttext") # more interested in semantics than syntactics
  dimensions <- c(128L) # 128 and 256 were comparable, go with cheaper 128
  window <- c(8L)
  min_word_occur <- c(20L)
  epochs <- c(5L)
  
  
  #### Old parameters
  
  ## Corpus hyperparameters here
  # remove_stopwords <- c(FALSE)
  # # remove_stopwords <- c(TRUE, FALSE)
  # preserve_code_references <- c(TRUE)
  # # preserve_code_references <- c(TRUE, FALSE)
  # preserve_ngrams <- c(TRUE)
  # # preserve_ngrams <- c(TRUE, FALSE)

  
  ## embedding hyperparameters here
  # type <- c("word2vec") # more interested in semantics than syntactics
  # # type <- c("word2vec", "fasttext")
  # dimensions <- c(128L) # 128 and 256 were comparable, go with cheaper 128
  # # dimensions <- c(64L, 128L, 256L)
  # # dimensions <- c(32L, 128L, 256L, 512L)
  # # dimensions <- c(32L, 64L, 96L, 128L, 192L, 256L, 512L)
  # window <- c(8L)
  # # window <- c(8L, 20L)
  # # window <- c(2L, 4L, 8L)
  # # window <- c(2L, 4L, 5L, 8L, 10L)
  # min_word_occur <- c(20L)
  # # min_word_occur <- c(5L, 10L, 20L, 50L)
  # # min_word_occur <- c(5L, 20L, 50L)
  # # min_word_occur <- c(3L, 5L, 10L, 20L, 50L)
  # epochs = c(5L)
  # # epochs = c(5L, 20L)
  # 
  corpus_param_df <- expand_grid(remove_stopwords, preserve_code_references, preserve_ngrams)


  ## this generates the corpus permutations and saves them to disk
  corpus_param_df <-  corpus_param_df %>% pmap_dfr(generate_corpus)

  embed_param_df <- expand_grid(corpus_param_df,
                                type,
                                dimensions,
                                window,
                                min_word_occur,
                                epochs)

  ## from each corpus generate N word embeddings, where N is the number of
  ## combinations of the embedding hyperparameters
  embed_param_df <- embed_param_df %>%
    pmap_dfr(train_gensim_wv)



  cat("Generated all embeddings. Saving tibble with information...\n")
  embedding_df <- expand_grid(corpus_param_df, embed_param_df)

  write_csv2(embedding_df, "./data/corpus_info.csv")

  return(embedding_df)

  }


## this function generates different versions of the corpus based on
## whether the user wants stopwords removed, preserve irc refs, etc.
generate_corpus <-
  function(remove_stopwords,
           preserve_code_references,
           preserve_ngrams) {
    ## IMPORTANT: I made the process_corpus function multisession, so multisession
    ## programming should not be used here when generating the corpus.
    
    filename <-
      paste0(
        "sw-",
        str_sub(tolower(!remove_stopwords), 1, 1),
        "_refs-",
        str_sub(tolower(preserve_code_references), 1, 1),
        "_ngrams-",
        str_sub(tolower(preserve_ngrams), 1, 1),
        ".pq"
      )
    # ".rds")
    
    prep_corpus_path <- file.path(PREP_CORPI_PATH, filename)
    
    cat("Generating corpus '", filename, "'...\n")
    
    if (file_exists(prep_corpus_path)) {
      # if the prepared corpus already exists on disk, load it
      cat("Corpus already exists on disk. Loading it...\n")
      corpus <- read_parquet(prep_corpus_path)
      
    } else {
      # if the prepare corpus does NOT exist on disk, load the raw corpus,
      # process it, and save it as a .pq file.
      corpus <- load_corpus()
      
      ## accepting defaults to preserve currency and percent phrases and not preserve ngrams
      message(str_glue("---Preparing corpus---"))
      corpus <-
        prepare_corpus(corpus,
                       preserve_references = preserve_code_references,
                       preserve_ngrams = preserve_ngrams)
      
      
      message(str_glue("Saving temp prepared corpus..."))
      # saveRDS(corpus, file.path("/data/rstudio/corpi/prepared/sw_refs_ngrams", filename), compress = FALSE)
      write_parquet(tibble(lines = corpus),
                    file.path(PREP_CORPI_PATH, filename))
    }
    
    tok_corpus_path <- file.path(PREP_CORPI_PATH, filename)
    if (file_exists(corpus_path)) {
    
    # accepting defaults to not stem words
    message(str_glue("---Tokenizing corpus---"))
    corpus <-
      tokenize_corpus(corpus, remove_stopwords = remove_stopwords)
    
    
    message(str_glue("--- Saving {corpus_path}...", TOK_CORPI_PATH))
    write_parquet(tibble(lines = corpus), file.path(TOK_CORPI_PATH, filename))
    
    # invisible(corpus)
    
    ## use this code if want to return a tibble with all parameters
    ## right now just writing corpi to disk so don't have to repeatedly
    ## generate the same corpus for different embedding options
    tibble(
      corpuspath = corpus_path,
      stopwords = !remove_stopwords,
      code_refs = preserve_code_references,
      preserve_ngrams = preserve_ngrams
    )
  }





train_gensim_wv <- function(corpuspath,
                            stopwords,
                            code_refs,
                            preserve_ngrams,
                            type = "word2vec",
                            dimensions = 300L,
                            window = 3L,
                            min_word_occur = 5L,
                            epochs = 5L) {

  embed_name <- paste(
    if_else(stopwords, "sw", "nosw"),
    if_else(code_refs, "refs", "norefs"),
    if_else(type == "word2vec", "w2v", "ft"),
    if_else(preserve_ngrams, "ngrams", "no_ngrams"),
    "d",
    dimensions,
    "w",
    window,
    "mwo",
    min_word_occur,
    "e",
    epochs,
    sep = "_"
  )

  cat("Training '", embed_name, "'...\n")

  embed_path <- file.path(EMBED_PATH, embed_name)

  embed_info <- tibble(
    embedding_path = embed_path,
    type = type,
    dimensions = dimensions,
    window = window,
    min_word_occur = min_word_occur,
    epochs = epochs,
    message = "Success"
  )

  if (file_exists(embed_path)) {
    cat(embed_name, " already created.\n")
    return(embed_info)
  }

  numCores <- 12L
  # numCores <- as.integer(parallel::detectCores() / 2)

  if (!file_exists(corpuspath)) {
    cat("Corpus (", corpuspath, ") not found.\n")
  } else {
    if (exists("corpus")) {
      cat("Corpus is already loaded...\n")
    }
    else {
      cat("Loading ", corpuspath, "...\n")
      # corpus <- readRDS(corpuspath)
      corpus <- read_parquet(corpuspath)
    }
  }
  
  tryCatch({
    
    cat("Creating ", embed_name, "embedding...\n")
    source_python("1_creation/python/train_embedding.py")
    train_embedding(embed_path, corpus_path, type, dimensions, min_word_occur, window, numCores, epochs)

  },

  error = function(e) {
    message("FAILURE: Training the word embedding failed!")
    message("Here is the error message:")
    message(e)
    # embed_info$message = cat("Failure! ", unlist(e))
    # return (embed_info)
  },

  warning = function(w) {
    message("WARNING: A warning occurred while training the word embedding.")
    message("Here is the warning message:")
    message(w)
    # embed_info$message = cat("Warning! ", unlist(w))
  },

  finally = {
    if (exists("corpus"))
      rm(corpus)
    if (exists("model"))
      rm(model)
    return(embed_info)
  })

}
