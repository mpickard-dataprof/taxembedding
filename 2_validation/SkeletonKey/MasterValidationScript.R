########################################################################################
#            |                                                                         #
# FILE:      | MasterValidationScript                                                  #
#------------|-------------------------------------------------------------------------#
# USE:       | This script performs pair, analogy, and variable evaluations            #
#------------|-------------------------------------------------------------------------#
# AUTHOR(S): | Alexander Wold (Z1817662)                                               #
#------------|-------------------------------------------------------------------------#
# DATE:      | 1/21/2021                                                               #
#            |                                                                         #
########################################################################################
##############################################################################
#          |                                                                 #
# SECTION: | CONSTANTS                                                       #
#----------|-----------------------------------------------------------------#
# USE:     | Here, the user can define essential constants                   #
#          |                                                                 #
##############################################################################
PYTHON_SCRIPT <- "/home/alex/Projects/taxembed/2_validation/SkeletonKey/ParallelMasterValidationScript.py"
VAR_PATH <- "/home/alex/Projects/taxembed/2_validation/SkeletonKey/output/best"
VAR_OUTPUT_PATH <- "/home/alex/Projects/taxembed/2_validation/SkeletonKey/output/best"


##############################################################################
#          |                                                                 #
# SECTION: | PACKAGES                                                        #
#----------|-----------------------------------------------------------------#
# USE:     | This section imports essential r packages                       #
#          |                                                                 #
##############################################################################
library(reticulate)
library(tibble)
library(stringr)
library(broom)
library(purrr)
library(dplyr)
library(pkgcond)


##############################################################################
#          |                                                                 #
# SECTION: | FUNCTIONS                                                       #
#----------|-----------------------------------------------------------------#
# USE:     | This section provides key functionality for the script          #
#          |                                                                 #
##############################################################################
################################################################
#           |                                                  #
# FUNCTION: | model_aov                                        #
#-----------|--------------------------------------------------#
# USE:      | this function reads model correlation data from  #
#           | a csv file and performs aov operations on each   #
#           | variable to see if there's a significant         #
#           | difference between variations in the word        #
#           | embedding                                        #
#           |                                                  #
################################################################
model_aov <- function(df)
{
  anova_pearson <- list()
  anova_spearman <- list()
  anova_analogy <- list()
  return_list <- list()
  
  # anova table for the stop words
  if(nlevels(as.factor(df$Stopwords.)) >= 2)
  {
    if ("Pearson.Correlation.Coefficient" %in% colnames(df)) {
      anova_pearson[["Stopwords"]] <- aov(Pearson.Correlation.Coefficient ~ Stopwords., data = df)
    }
    if ("Spearman.Correlation.Coefficient" %in% colnames(df)) {
      anova_spearman[["Stopwords"]] <- aov(Spearman.Correlation.Coefficient ~ Stopwords., data = df)
    }
    if ("Overall.Analogy.Accuracy...." %in% colnames(df)) {
      anova_analogy[["Stopwords"]] <- aov(Overall.Analogy.Accuracy.... ~ Stopwords., data = df)
    }
  }
  
  # anova table for references
  if (nlevels(as.factor(df$References.Preserved.)) >= 2)
  {
    if ("Pearson.Correlation.Coefficient" %in% colnames(df)) {
      anova_pearson[["References"]] <- aov(Pearson.Correlation.Coefficient ~ References.Preserved., data = df)
    }
    if ("Spearman.Correlation.Coefficient" %in% colnames(df)) {
      anova_spearman[["References"]] <- aov(Spearman.Correlation.Coefficient ~ References.Preserved., data = df)
    }
    if ("Overall.Analogy.Accuracy...." %in% colnames(df)) {
      anova_analogy[["References"]] <- aov(Overall.Analogy.Accuracy.... ~ References.Preserved., data = df)
    }
  }
  
  # anova table for the training algorithm
  if (nlevels(as.factor(df$Training.Algorithm)) >= 2)
  {
    if ("Pearson.Correlation.Coefficient" %in% colnames(df)) {
      anova_pearson[["Algorithm"]] <- aov(Pearson.Correlation.Coefficient ~ Training.Algorithm, data = df)
    }
    if ("Spearman.Correlation.Coefficient" %in% colnames(df)) {
      anova_spearman[["Algorithm"]] <- aov(Spearman.Correlation.Coefficient ~ Training.Algorithm, data = df)
    }
    if ("Overall.Analogy.Accuracy...." %in% colnames(df)) {
      anova_analogy[["Algorithm"]] <- aov(Overall.Analogy.Accuracy.... ~ Training.Algorithm, data = df)
    }
  }
  
  # anova table for the ngrams
  if (nlevels(as.factor(df$Ngrams.)) >= 2)
  {
    if ("Pearson.Correlation.Coefficient" %in% colnames(df)) {
      anova_pearson[["Ngram"]] <- aov(Pearson.Correlation.Coefficient ~ Ngrams., data = df)
    }
    if ("Spearman.Correlation.Coefficient" %in% colnames(df)) {
      anova_spearman[["Ngram"]] <- aov(Spearman.Correlation.Coefficient ~ Ngrams., data = df)
    }
    if ("Overall.Analogy.Accuracy...." %in% colnames(df)) {
      anova_analogy[["Ngram"]] <- aov(Overall.Analogy.Accuracy.... ~ Ngrams., data = df)
    }
  }
  
  # anova table for the number of dimensions
  if (nlevels(as.factor(df$Dimensions)) >= 2)
  {
    if ("Pearson.Correlation.Coefficient" %in% colnames(df)) {
      anova_pearson[["Dimensions"]] <- aov(Pearson.Correlation.Coefficient ~ Dimensions, data = df)
    }
    if ("Spearman.Correlation.Coefficient" %in% colnames(df)) {
      anova_spearman[["Dimensions"]] <- aov(Spearman.Correlation.Coefficient ~ Dimensions, data = df)
    }
    if ("Overall.Analogy.Accuracy...." %in% colnames(df)) {
      anova_analogy[["Dimensions"]] <- aov(Overall.Analogy.Accuracy.... ~ Dimensions, data = df)
    }
  }
  
  # anova table for the window size
  if (nlevels(as.factor(df$Window)) >= 2)
  {
    if ("Pearson.Correlation.Coefficient" %in% colnames(df)) {
      anova_pearson[["Window"]] <- aov(Pearson.Correlation.Coefficient ~ Window, data = df)
    }
    if ("Spearman.Correlation.Coefficient" %in% colnames(df)) {
      anova_spearman[["Window"]] <- aov(Spearman.Correlation.Coefficient ~ Window, data = df)
    }
    if ("Overall.Analogy.Accuracy...." %in% colnames(df)) {
      anova_analogy[["Window"]] <- aov(Overall.Analogy.Accuracy.... ~ Window, data = df)
    }
  }
  
  # anova table for the minimum count threshold
  if (nlevels(as.factor(df$Minimum.Count.Threshold)) >= 2)
  {
    if ("Pearson.Correlation.Coefficient" %in% colnames(df)) {
      anova_pearson[["Threshold"]] <- aov(Pearson.Correlation.Coefficient ~ Minimum.Count.Threshold, data = df)
    }
    if ("Spearman.Correlation.Coefficient" %in% colnames(df)) {
      anova_spearman[["Threshold"]] <- aov(Spearman.Correlation.Coefficient ~ Minimum.Count.Threshold, data = df)
    }
    if ("Overall.Analogy.Accuracy...." %in% colnames(df)) {
      anova_analogy[["Threshold"]] <- aov(Overall.Analogy.Accuracy.... ~ Minimum.Count.Threshold, data = df)
    }
  }
  
  # anova table for the numnber of training epochs
  if (nlevels(as.factor(df$Number.of.Training.Epochs)) >= 2)
  {
    if ("Pearson.Correlation.Coefficient" %in% colnames(df)) {
      anova_pearson[["Epochs"]] <- aov(Pearson.Correlation.Coefficient ~ Number.of.Training.Epochs, data = df)
    }
    if ("Spearman.Correlation.Coefficient" %in% colnames(df)) {
      anova_spearman[["Epochs"]] <- aov(Spearman.Correlation.Coefficient ~ Number.of.Training.Epochs, data = df)
    }
    if ("Overall.Analogy.Accuracy...." %in% colnames(df)) {
      anova_analogy[["Epochs"]] <- aov(Overall.Analogy.Accuracy.... ~ Number.of.Training.Epochs, data = df)
    }
  }
  
  # return the appropriate data based on the pearson and spearman argument flags
  if ("Pearson.Correlation.Coefficient" %in% colnames(df))
  {
    return_list[["pearson"]] <- anova_pearson
  }
  
  if ("Spearman.Correlation.Coefficient" %in% colnames(df))
  {
    return_list[["spearman"]] <- anova_spearman
  }
  
  if ("Overall.Analogy.Accuracy...." %in% colnames(df))
  {
    return_list[["analogy"]] <- anova_analogy
  }
  
  return_list
}


################################################################
#           |                                                  #
# FUNCTION: | apply_tukey                                      #
#-----------|--------------------------------------------------#
# USE:      | given a list of aov fitted objects, this         #
#           | function applies the tukeyHSD function to each   #
#           | element                                          #
#           |                                                  #
################################################################
apply_tukey <- function(aov_list)
{
  tukey_list <- lapply(aov_list, TukeyHSD)
  tukey_list
}


################################################################
#           |                                                  #
# FUNCTION: | get_tukey                                        #
#-----------|--------------------------------------------------#
# USE:      | given a list of aov lists, this function applies #
#           | the apply_tukey function to each element         #
#           |                                                  #
################################################################
get_tukey <- function(list_of_aov_lists)
{
  tukey_list <- lapply(list_of_aov_lists, apply_tukey)
  tukey_list
}

################################################################
#           |                                                  #
# FUNCTION: | get_broom                                        #
#-----------|--------------------------------------------------#
# USE:      | given a list of tukey outputs, this function     #
#           | uses the tidy function and the reduce function   #
#           | to display the variable tests in an easy-to-read #
#           | format                                           #
#           |                                                  #
################################################################
get_broom <- function(list_of_tukey_tables)
{
  broom_out <- invisible(lapply(list_of_tukey_tables, tidy))
  broom_out <- suppress_messages(reduce(broom_out, full_join))
  broom_out
}

################################################################
#           |                                                  #
# FUNCTION: | print_tibble                                     #
#-----------|--------------------------------------------------#
# USE:      | given a list of tibbles, this function           #
#           | prints them out, one by one                      #
#           |                                                  #
################################################################
print_tibble <- function(list_of_tibbles)
{
  # loop through the provided list and print the tibble information
  for(i in seq(1, length(list_of_tibbles)))
  {
    print(names(list_of_tibbles)[[i]])
    
    print_frame <- as.data.frame(list_of_tibbles[[i]]) %>% mutate_if(is.numeric, round, digits = 4)
    
    print(print_frame)
    cat("\n\n\n")
  }
}


##############################################################################
#          |                                                                 #
# SECTION: | DRIVER                                                          #
#----------|-----------------------------------------------------------------#
# USE:     | This section calls a python script to evaluate word embeddings  #
#          |                                                                 #
##############################################################################
##############################################################################
#          |                                                                 #
#          | PAIR/ANALOGY EVALUATION                                         #
#          |                                                                 #
##############################################################################
# evaluate word embeddings
py_run_file(PYTHON_SCRIPT)


##############################################################################
#          |                                                                 #
#          | VARIABLE TESTS                                                  #
#          |                                                                 #
##############################################################################
# recursively acquire the names of all evaluations, 
all_evals <- list.files(VAR_PATH, full.names = TRUE, recursive = TRUE)

# extract a list of names for the evaluation files
var_paths <- c()
for (i in seq(1, length(all_evals))) {
  ename <- tail(unlist(str_split(string = all_evals[i], pattern = "/")), n = 1)
  ename <- head(unlist(str_split(string = ename, pattern = "\\.")), n = 1)
  ename <- paste0(ename, "_VARTEST.txt")
  ename <- paste0(VAR_OUTPUT_PATH, "/", ename)
  var_paths <- append(var_paths, ename)
}

# loop through all evaluation files
for (k in seq(1, length(all_evals))) {
  cor_frame <- read.csv(all_evals[k], header = TRUE)
  possible_factors <- c("Dimensions", "Window", "Minimum.Count.Threshold", "Number.of.Training.Epochs")
  col_factors <- possible_factors[possible_factors %in% names(cor_frame)]
  cor_frame[ , col_factors] <- data.frame(apply(cor_frame[col_factors], 2, as.factor))
  
  # get the tukeyHSD information
  variables_aov <- model_aov(cor_frame)
  hsd_list <- get_tukey(variables_aov)
  out_list <- lapply(hsd_list, get_broom)
  
  # print the tukeyHSD information
  options(width = 1000)
  sink(var_paths[k])
  print_tibble(out_list)
  sink()
}