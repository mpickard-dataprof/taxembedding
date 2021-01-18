########################################################################################
#            |                                                                         #
# FILE:      | POS_Tagging.R                                                           #
#------------|-------------------------------------------------------------------------#
# USE:       | This file calls the POS_Tagging.py script to generate a part-of-speech  #
#            | training model from each of our gensim word embeddings. We use these    #
#            | networks to evaluate overall performance on downstream tasks            #
#------------|-------------------------------------------------------------------------#
# AUTHOR(S): | Alexander Wold (Z1817662)                                               #
#------------|-------------------------------------------------------------------------#
# DATE:      | October 22, 2020                                                        #
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
TEXT_PATH <- "/home/mpickard/Projects/ustax/data/irc26.rda"
NLP_TRAIN_TEST_OUT_PATH <- "/home/mpickard/Projects/ustax/data/Train_and_Test/irc26_corpus.txt"
PYTHON_POS_SCRIPT <- "/home/mpickard/Projects/ustax/Extrinsic_Evaluators/POS_Tagging/POS_Tagging.py"


##############################################################################
#          |                                                                 #
# SECTION: | PACKAGES                                                        #
#----------|-----------------------------------------------------------------#
# USE:     | loads the required packages                                     #
#          |                                                                 #
##############################################################################
library(cleanNLP)
library(openNLP)
library(NLP)
library(openNLPmodels.en)
library(reticulate)


##############################################################################
#          |                                                                 #
# SECTION: | DRIVER                                                          #
#----------|-----------------------------------------------------------------#
# USE:     | creates training and testing data from the NLP taggers and      #
#          | calls a python script to generate a model from the data         #
#          |                                                                 #
##############################################################################
# load in the data
load(TEXT_PATH)

# convert the text data into a workable string format for POS tagging from the openNLP package
corpus <- as.String(c(irc26[[1]], irc26[[2]], irc26[[3]], irc26[[4]], irc26[[5]], irc26[[6]], irc26[[7]]))

# open a file connection to save training and testing data for POS tagging
file.create(NLP_TRAIN_TEST_OUT_PATH)

# write to the files
writeLines(corpus, NLP_TRAIN_TEST_OUT_PATH)

# call the python script to use this training and testing data to construct a POS keras model
py_run_file(PYTHON_POS_SCRIPT)
