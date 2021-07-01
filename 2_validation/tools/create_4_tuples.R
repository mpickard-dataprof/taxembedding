library(fs)

combine_tuples <- function(filepath) {
  require(readr)
  print(filepath)
  # if (!str_detect(filepath, "\\.csv$"))
  if (!str_detect(filepath, "\\.txt$"))
    return
  
  # df <- read_csv(filepath, col_names = FALSE)
  df <- read_tsv(filepath, col_names = FALSE, skip = 1)
  
  ## if odd num of rows, drop one
  if (nrow(df) %% 2) {
    df <- df[-1, ]
  }
  
  # randomly select half the rows
  x <- sample(nrow(df) / 2)
  
  filename <- path_file(filepath) %>% path_ext_remove()
  
  filepath_out <- filepath %>% 
    str_replace("2", "4") 
  # str_replace("cleaned/", "") %>% 
  # str_replace("csv$", "txt") %>% 
  # str_replace(filename, str_c("analogy_syntactic_tax_unigram_", filename))
  
  # glue the columns of two halves together
  new_df <- bind_cols(df[x, ], df[-x, ]) %>% 
    # set_colnames(paste0("word", 1:4))  %>%
    write_tsv(filepath_out)
  
}

dir_walk("~/Projects/pos_tagging/_sandbox/data/bats/2-tuples",
         combine_tuples,
         type = "file")
