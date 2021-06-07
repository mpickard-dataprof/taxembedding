########################################################################################
#            |                                                                         #
# FILE:      | Correlations.py                                                         #
#------------|-------------------------------------------------------------------------#
# USE:       | This file handles the python functionality of the Correlations.R file   #
#------------|-------------------------------------------------------------------------#
# AUTHOR(S): | Alexander Wold                                                          #
#------------|-------------------------------------------------------------------------#
# DATE:      | September 16, 2020                                                      #
#            |                                                                         #
########################################################################################


##############################################################################
#          |                                                                 #
# SECTION: | USER VARIABLES                                                  #
#----------|-----------------------------------------------------------------#
# USE:     | let the user define some key variables that will change the     #
#          | behaviour of the script                                         #
#          |                                                                 #
##############################################################################
MODEL_PATH = "/data/rstudio/embeddings"
OUT_TABLE_PATH = "/home/alex/Projects/taxembed/data/output/validations/custom_ngram_similarity_custom_ngram_analogies_1_6_2021.csv"
SIM_REL_PAIRS_PATH = "/home/alex/Projects/taxembed/data/output/tallied/pairs/ngram_sim"
ANALOGIES_PATH = "/home/alex/Projects/taxembed/data/output/tallied/analogies/analogies-ngrams"
NGRAMS = True


##############################################################################
#          |                                                                 #
# SECTION: | PACKAGES                                                        #
#----------|-----------------------------------------------------------------#
# USE:     | load the necessary packages for implementation                  #
#          |                                                                 #
##############################################################################
import numpy as np
import pandas as pd
import os
import gensim
import re
import tabulate
from IPython.display import display, HTML


##############################################################################
#          |                                                                 #
# SECTION: | FUNCTIONS                                                       #
#----------|-----------------------------------------------------------------#
# USE:     | provides fundamental functionality to the script                #
#          |                                                                 #
##############################################################################
################################################################
#           |                                                  #
# FUNCTION: | print_pair_eval                                  #
#-----------|--------------------------------------------------#
# USE:      | prints the results of an evalutation object      #
#           |                                                  #
################################################################
def print_pair_eval(eval_dict):
    
    # print the pearson correlation coefficient and the pvalue
    print("Pearson Correlation Coefficient:", eval_dict["p_c"])
    print("Pearson Correlation Coefficient 2-Tailed P-Value:", eval_dict["p_p"])

    # print the spearman correlation coefficient and the pvalue
    print("Spearman Correlation Coefficient:", eval_dict["s_c"])
    print("Spearman Correlation Coefficient 2-Tailed P-Value:", eval_dict["s_p"])
    
    # print the ratio of pairs with unknown words
    print("Ratio of Phrase Pairs with Unknown Words:", eval_dict["u"])

################################################################
#           |                                                  #
# FUNCTION: | store_pair_eval                                  #
#-----------|--------------------------------------------------#
# USE:      | stores the results of an evaluation object into  #
#           | a dictionary                                     #
#           |                                                  #
################################################################
def store_pair_eval(eval_object, analogy_score):

    # create a more discriptive dictionary for the values of a word pair evaluation
    eval_dict = {"Pearson Correlation Coefficient" : eval_object[0][0],
                 "Pearson Correlation Coefficient P-Value" : eval_object[0][1],
                 "Spearman Correlation Coefficient" : eval_object[1].correlation,
                 "Spearman Correlation Coefficient P-Value" : eval_object[1].pvalue,
                 "Unknown Word Pairs" : eval_object[2],
                 "Overall Analogy Accuracy (%)" : analogy_score[0] * 100}
    
    return eval_dict

################################################################
#           |                                                  #
# FUNCTION: | load_evals                                       #
#-----------|--------------------------------------------------#
# USE:      | systematically loads embedding models and grabs  #
#           | the word evaluation information.                 #
#           |                                                  #
################################################################
def load_evals(model_list):
    
    # create an empty list to store the evaluation dictionaries
    eval_list = []
    
    for path in model_list:
        loaded_model = gensim.models.Word2Vec.load(path)
        eval_list.append(store_pair_eval(loaded_model.wv.evaluate_word_pairs(SIM_REL_PAIRS_PATH),
                                                                             loaded_model.wv.evaluate_word_analogies(ANALOGIES_PATH)))
        del loaded_model
        
    return eval_list

################################################################
#           |                                                  #
# FUNCTION: | name_scrape                                      #
#-----------|--------------------------------------------------#
# USE:      | grabs information from the model file name and   #
#           | returns a dictionary with all of the information #
#           |                                                  #
################################################################
def name_scrape(file_name):
    
    stopwords_flag = False
    code_refs_flag = False
    embedding_type_flag = "Word2Vec"
    ngrams_flag = False
    
    # check if the model used stopwords
    if re.search(pattern = "nosw_", string = file_name) == None:
        stopwords_flag = True
    
    # check if the model preserved custom references
    if re.search(pattern = "_refs_", string = file_name) != None:
        code_refs_flag = True
        
    # check the algorithm used for training
    if re.search(pattern = "_ft_", string = file_name) != None:
        embedding_type_flag = "FastText"
        
    else:
        embedding_type_flag = "Word2Vec"
        
    # check if the model preserved ngrams
    if re.search(pattern = "_no_ngrams_", string = file_name) == None:
        ngrams_flag = True
    
    
    # grab information for the dimensions
    dimensions_flag = int(re.search(pattern = "\d{1,3}", string = re.search(pattern = "d_\d{1,3}_",
                                                                            string = file_name).group(0)).group(0))
    
    # grab information for the window
    window_flag = int(re.search(pattern = "\d{1,3}", string = re.search(pattern = "_w_\d{1,3}", string = file_name).group(0)).group(0))
    
    # grab information for the min word occur
    mwo_flag = int(re.search(pattern = "\d{1,3}", string = re.search(pattern = "_mwo_\d{1,3}", string = file_name).group(0)).group(0))
    
    # grab information for the epochs
    epoch_flag = int(re.search(pattern = "\d{1,4}", string = re.search(pattern = "_e_\d{1,4}", string = file_name).group(0)).group(0))
    
    
    # create a dictionary with all of the information
    title_dict = {"Stopwords?" : stopwords_flag, "References Preserved?" : code_refs_flag,
                 "Training Algorithm" : embedding_type_flag, "Ngrams?" : ngrams_flag,
                 "Dimensions" : dimensions_flag, "Window" : window_flag,
                 "Minimum Count Threshold" : mwo_flag, "Number of Training Epochs" : epoch_flag}
    
    return(title_dict)


##############################################################################
#          |                                                                 #
# SECTION: | DRIVER                                                          #
#----------|-----------------------------------------------------------------#
# USE:     | applies the functions defined above to create and save a data   #
#          | table with all of the model correlation data                    #
#          |                                                                 #
##############################################################################
# get the list of models from the user specified path
# use list comprehension tp get a list of models within the 'models' folder
# do not list the peripheral .npy files
model_list = [file for file in os.listdir(MODEL_PATH) if re.match("^(?:(?!\.).)*$", file)]

if NGRAMS == True:
    model_list = [modelname for modelname in model_list if re.search("(?<!no_)ngrams", modelname)]

# create an empty list to hold the path names for each model
model_paths = []

# append the current working directory to each model name
for model_name in model_list:
    model_paths.append(os.path.join(MODEL_PATH, model_name))
    
# load each of the models
summary_evals = load_evals(model_paths)

# create an empty list to store dictionaries built from model file names
name_info_dicts = []

# create a series of dictionaries taken from the information contained in a model file title
for name in model_list:
    name_info_dicts.append(name_scrape(name))

# merge the summary evaluation statistics with the name information
for i in range(len(name_info_dicts)):
    name_info_dicts[i].update(summary_evals[i])
    
# create a list of key names (column names) and an empty list to hold header names
column_names = name_info_dicts[0].keys()
header_names = []

# take the key names and replace all spaces with \n, this will allow for better printing with tabulate
for col_name in column_names:
    header_names.append(re.sub(pattern = "\s", repl = "\n", string = col_name))

# create an empty data frame with the specified key (column) names
info_df = pd.DataFrame(columns = column_names)

# append the information to the data frame
for i in range(len(name_info_dicts)):
    info_df = info_df.append(name_info_dicts[i], ignore_index = True)

# write the pandas data frame to a csv file
info_df.to_csv(OUT_TABLE_PATH)
