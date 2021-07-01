source("1_creation/R/process_corpus.R")


library(dplyr)
library(fs)
library(readr)
library(purrr)
library(tidyr)

## PATHS FOR OUTPUTS
EMBED_PATH = '/data/rstudio/embeddings'
CORPI_PATH = '/data/rstudio/corpi'

generateEmbeddings <- function() {

  # num_cores <- availableCores() / 2
  num_cores <- 12

  ## check if 'corpi' and 'embeddings' directories exists
  ## if not, create them
  if (!dir_exists(CORPI_PATH)) {
    cat("'corpi' directory does not exist. Creating it...\n")
    dir_create(CORPI_PATH)
  }

  if (!dir_exists(EMBED_PATH)) {
    cat("'embeddings' directory does not exist. Creating it...\n")
    dir_create(EMBED_PATH)
  }

  ## Corpus hyperparameters here
  remove_stopwords <- c(TRUE, FALSE)
  preserve_code_references <- c(TRUE, FALSE)
  preserve_ngrams <- c(TRUE, FALSE)

  ## embedding hyperparameters here
  type <- c("word2vec", "fasttext")
  dimensions <- c(64L, 128L, 256L)
  # dimensions <- c(32L, 128L, 256L, 512L)
  # dimensions <- c(32L, 64L, 96L, 128L, 192L, 256L, 512L)
  window <- c(8L, 20L)
  # window <- c(2L, 4L, 8L)
  # window <- c(2L, 4L, 5L, 8L, 10L)
  min_word_occur <- c(5L, 10L, 20L, 50L)
  # min_word_occur <- c(5L, 20L, 50L)
  # min_word_occur <- c(3L, 5L, 10L, 20L, 50L)
  epochs = c(5L, 20L)

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
generate_corpus <- function(remove_stopwords, preserve_code_references, preserve_ngrams) {

  ## IMPORTANT: I made the process_corpus function multisession, so multisession
  ## programming should not be used here when generating the corpus.
  
  filename <-  paste0("sw-", str_sub(tolower(!remove_stopwords), 1, 1),
                      "_refs-", str_sub(tolower(preserve_code_references), 1, 1),
                      "_ngrams-", str_sub(tolower(preserve_ngrams), 1, 1))

  corpus_path <- file.path(CORPI_PATH, filename)

  cat("Generating corpus '", filename, "'...\n")

  if (file_exists(corpus_path)) {

    cat("Corpus already exists on disk.\n")

  } else {

    cat(corpus_path, ": Creating corpus...\n")
    ## load the raw corpus from disk
    corpus <- load_corpus()

    ## accepting defaults to preserve currency and percent phrases and not preserve ngrams
    corpus <-
      prepare_corpus(corpus, preserve_references = preserve_code_references, preserve_ngrams = preserve_ngrams)

    ## accepting defaults to not stem words
    corpus <-
      tokenize_corpus(corpus, remove_stopwords = remove_stopwords)

    saveRDS(corpus, corpus_path, compress = FALSE)
  }

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
  library(reticulate)
  library(dplyr)

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
    cat("Loading ", corpuspath, "...\n")
    corpus <- readRDS(corpuspath)
  }

  tryCatch({
    cat("Creating ", embed_name, " embedding...\n")

    # PYTHON = from gensim import word2vec, fasttext
    gensim <- import("gensim")

    type <- match.arg(type, c("fasttext", "word2vec"))

    if (type == "word2vec") {
      # create Word2Vec using Python call to gensim's word2vec implementation
      model <- gensim$models$word2vec$Word2Vec(
        size = dimensions,
        window = window,
        min_count = min_word_occur,
        workers = numCores
      )
    }

    if (type == "fasttext") {
      # create Fasttext using Python call to gensim's word2vec implementation
      model <- gensim$models$fasttext$FastText(
        size = dimensions,
        window = window,
        min_count = min_word_occur,
        workers = numCores
      )
    }

    model$build_vocab(sentences = corpus)

    model$train(
      sentences = corpus,
      total_examples =  length(corpus),
      epochs = epochs
    )


    cat("Saving ", embed_name, " embedding...\n")
    # pickle version 4 compresses files more, but may cause problems
    model$save(embed_path, pickle_protocol=4)

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
