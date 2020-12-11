source("R/process_corpus.R")
source("R/load_corpus.R")

library(dplyr)
library(fs)
library(readr)

generateEmbeddings <- function() {

  library(tidyr)
  library(furrr)
  library(purrr)
  plan(multiprocess)


  num_cores <- availableCores() / 2
  plan(multiprocess, workers = num_cores)

  ## check if 'corpi' and 'embeddings' directories exists
  ## if not, create them
  if (!dir_exists("./data/corpi")) {
    cat("'corpi' directory does not exist. Creating it...\n")
    dir_create("./data/corpi")
  }

  if (!dir_exists("./data/embeddings")) {
    cat("'embeddings' directory does not exist. Creating it...\n")
    dir_create("./data/embeddings")
  }

  ## Corpus hyperparameters here
  remove_stopwords <- c(TRUE, FALSE)
  preserve_code_references <- c(FALSE)
  preserve_ngrams <- c(FALSE)

  ## embedding hyperparameters here
  type <- c("word2vec")
  dimensions <- c(128L)
  window <- c(4L)
  min_word_occur <- c(5L)
  epochs = c(5L)

  corpus_param_df <- expand_grid(remove_stopwords, preserve_code_references, preserve_ngrams)


  ## this generates the corpus permutations and saves them to disk
  corpus_param_df <-  corpus_param_df %>% future_pmap_dfr(generate_corpus)
  # corpus_param_df %>% future_pwalk(generate_corpi)

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

  filename <-  paste0("sw-", str_sub(tolower(!remove_stopwords), 1, 1),
                      "_refs-", str_sub(tolower(preserve_code_references), 1, 1),
                      "_ngrams-", str_sub(tolower(preserve_ngrams), 1, 1))

  corpus_path <- file.path("data/corpi", filename)

  cat("Generating corpus '", filename, "'...\n")

  if (file_exists(corpus_path)) {

    cat("Corpus already exists on disk.\n")

  } else {

    cat(corpus_path, ": Creating corpus...\n")
    ## load the corpus from disk
    corpus <- load_corpus()

    ## accepting defaults to preserve currency and percent phrases and not preserve ngrams
    corpus <-
      prepare_corpus(corpus, preserve_references = preserve_code_references, preserve_ngrams = preserve_ngrams)

    ## accepting defaults to not stem words
    corpus <-
      tokenize_corpus(corpus, remove_stopwords = remove_stopwords)

    saveRDS(corpus, corpus_path)
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

  embed_path <- file.path("data/embeddings", embed_name)

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

  numCores <- parallel::detectCores()

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
    model$save(embed_path)
    # saveRDS(model, embed_path)

  },

  error = function(e) {
    embed_info$message = cat("Failure! ", e)
    return (embed_info)
  },

  warning = function(w) {
    embed_info$message = cat("Warning! ", w)
  },

  finally = {
    if (exists("corpus"))
      rm(corpus)
    if (exists("model"))
      rm(model)
    return(embed_info)
  })

}
