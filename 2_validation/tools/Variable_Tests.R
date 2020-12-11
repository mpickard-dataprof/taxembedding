########################################################################################
#            |                                                                         #
# FILE:      | Variable_Tests.R                                                        #
#------------|-------------------------------------------------------------------------#
# USE:       | This script computes the anova and tukeyhsd information for all         #
#            | variables belonging to a word embedding. Essentially, these tests tell  #
#            | us whether changing certain variables significantly affects our word    #
#            | embedding performance.                                                  #
#------------|-------------------------------------------------------------------------#
# AUTHOR(S): | Alexander Wold (Z1817662)                                               #
#------------|-------------------------------------------------------------------------#
# DATE:      | 7/30/2020                                                               #
#            |                                                                         #
########################################################################################


##############################################################################
#          |                                                                 #
# SECTION: | OVERHEAD CONSTANTS                                              #
#----------|-----------------------------------------------------------------#
# USE:     | Change the variables in the section to manipulate the behaviour #
#          | of this script                                                  #
#          |                                                                 #
##############################################################################
CORRELATION_FILE_PATH = "/home/mpickard/Projects/ustax/data/output_data/correlations.csv"
OUTPUT_FILE_PATH = "/home/mpickard/Projects/ustax/data/output_data/anova_tukey_variables.txt"


##############################################################################
#          |                                                                 #
# SECTION: | FUNCTIONS                                                       #
#----------|-----------------------------------------------------------------#
# USE:     | provides fundamental functionality to the script                #
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
model_aov <- function(df, pearson = TRUE, spearman = TRUE, analogy = TRUE)
{
  anova_pearson <- list()
  anova_spearman <- list()
  anova_analogy <- list()
  return_list <- list()

  # anova table for the stop words
  if(nlevels(df$Stopwords.) >= 2)
  {
    anova_pearson[["Stopwords"]] <- aov(Pearson.Correlation.Coefficient ~ Stopwords., data = df)
    anova_spearman[["Stopwords"]] <- aov(Spearman.Correlation.Coefficient ~ Stopwords., data = df)
    anova_analogy[["Stopwords"]] <- aov(Overall.Analogy.Accuracy.... ~ Stopwords., data = df)
  }

  # anova table for references
  if (nlevels(df$References.Preserved.) >= 2)
  {
    anova_pearson[["References"]] <- aov(Pearson.Correlation.Coefficient ~ References.Preserved., data = df)
    anova_spearman[["References"]] <- aov(Spearman.Correlation.Coefficient ~ References.Preserved., data = df)
    anova_analogy[["References"]] <- aov(Overall.Analogy.Accuracy.... ~ References.Preserved., data = df)
  }


  # anova table for the training algorithm
  if (nlevels(df$Training.Algorithm) >= 2)
  {
    anova_pearson[["Algorithm"]] <- aov(Pearson.Correlation.Coefficient ~ Training.Algorithm, data = df)
    anova_spearman[["Algorithm"]] <- aov(Spearman.Correlation.Coefficient ~ Training.Algorithm, data = df)
    anova_analogy[["Algorithm"]] <- aov(Overall.Analogy.Accuracy.... ~ Training.Algorithm, data = df)
  }

  # anova table for the number of dimensions
  if (nlevels(df$Dimensions) >= 2)
  {
    anova_pearson[["Dimensions"]] <- aov(Pearson.Correlation.Coefficient ~ Dimensions, data = df)
    anova_spearman[["Dimensions"]] <- aov(Spearman.Correlation.Coefficient ~ Dimensions, data = df)
    anova_analogy[["Dimensions"]] <- aov(Overall.Analogy.Accuracy.... ~ Dimensions, data = df)

  }

  # anova table for the window size
  if (nlevels(df$Window) >= 2)
  {
    anova_pearson[["Window"]] <- aov(Pearson.Correlation.Coefficient ~ Window, data = df)
    anova_spearman[["Window"]] <- aov(Spearman.Correlation.Coefficient ~ Window, data = df)
    anova_analogy[["Window"]] <- aov(Overall.Analogy.Accuracy.... ~ Window, data = df)
  }

  # anova table for the minimum count threshold
  if (nlevels(df$Minimum.Count.Threshold) >= 2)
  {
    anova_pearson[["Threshold"]] <- aov(Pearson.Correlation.Coefficient ~ Minimum.Count.Threshold, data = df)
    anova_spearman[["Threshold"]] <- aov(Spearman.Correlation.Coefficient ~ Minimum.Count.Threshold, data = df)
    anova_analogy[["Threshold"]] <- aov(Overall.Analogy.Accuracy.... ~ Minimum.Count.Threshold, data = df)
  }

  # anova table for the numnber of training epochs
  if (nlevels(df$Number.of.Training.Epochs) >= 2)
  {
    anova_pearson[["Epochs"]] <- aov(Pearson.Correlation.Coefficient ~ Number.of.Training.Epochs, data = df)
    anova_spearman[["Epochs"]] <- aov(Spearman.Correlation.Coefficient ~ Number.of.Training.Epochs, data = df)
    anova_analogy[["Epochs"]] <- aov(Overall.Analogy.Accuracy.... ~ Number.of.Training.Epochs, data = df)
  }

  # return the appropriate data based on the pearson and spearman argument flags
  if (pearson == TRUE)
  {
    return_list[["pearson"]] <- anova_pearson
  }

  if (spearman == TRUE)
  {
    return_list[["spearman"]] <- anova_spearman
  }

  if (analogy == TRUE)
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
  lapply(aov_list, TukeyHSD)
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


##############################################################################
#          |                                                                 #
# SECTION: | DRIVER                                                          #
#----------|-----------------------------------------------------------------#
# USE:     | applies the functions defined above to calculate the tukeyhsd   #
#          | information pertaining to all variables for the word embeddings #
#          |                                                                 #
##############################################################################
# Read in the data
# read in the csv file for correlation information
cor_frame <- read.csv(CORRELATION_FILE_PATH, header = TRUE)
col_factors <- c("Dimensions", "Window", "Minimum.Count.Threshold", "Number.of.Training.Epochs")
cor_frame[ , col_factors] <- data.frame(apply(cor_frame[col_factors], 2, as.factor))

# get the tukeyHSD information
variables_aov <- model_aov(cor_frame)

# print the tukeyHSD information
sink(OUTPUT_FILE_PATH)
get_tukey(variables_aov)
sink()
