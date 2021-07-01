########################################################################################
#            |                                                                         #
# FILE:      | MasterValidationScript.py                                               #
#------------|-------------------------------------------------------------------------#
# USE:       | Automates the validation process for word embeddings                    #
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
# define important input paths the user should change
#MODEL_PATH = "/data/rstudio/embeddings"
MODEL_PATH = "/home/alex/Projects/taxembed/data/embeddings"
ANALOGY_PATH = "/home/alex/Projects/taxembed/2_validation/SkeletonKey/input/analogies"
PAIR_PATH = "/home/alex/Projects/taxembed/2_validation/SkeletonKey/input/wordpairs"

# define important output paths the user should change
ANALOGY_OUTPUT_PATH = "/home/alex/Projects/taxembed/2_validation/SkeletonKey/output/analogies"
PAIR_OUTPUT_PATH = "/home/alex/Projects/taxembed/2_validation/SkeletonKey/output/pairs"

# define important flags the user should change
NGRAMS = False


##############################################################################
#          |                                                                 #
# SECTION: | MODULES                                                         #
#----------|-----------------------------------------------------------------#
# USE:     | This section imports essential python modules                   #
#          |                                                                 #
##############################################################################
import os
import re
import gensim
import pandas as pd
import sys


##############################################################################
#          |                                                                 #
# SECTION: | GENERAL FUNCTIONS                                               #
#----------|-----------------------------------------------------------------#
# USE:     | This section defines general functions                          #
#          |                                                                 #
##############################################################################
################################################################
#           |                                                  #
# FUNCTION: | name_parse                                       #
#-----------|--------------------------------------------------#
# USE:      | grabs information from the model file name and   #
#           | returns a dictionary with all of the information #
#           |                                                  #
################################################################
def name_parse(model_path):
  
  # get the model name from the model path
  model_name = (model_path.split("/"))[-1]
  
  stopwords_flag = False
  code_refs_flag = False
  embedding_type_flag = "Word2Vec"
  ngrams_flag = False
  
  # check if the model used stopwords
  if re.search(pattern = "nosw_", string = model_name) == None:
      stopwords_flag = True
  
  # check if the model preserved custom references
  if re.search(pattern = "_refs_", string = model_name) != None:
      code_refs_flag = True
      
  # check the algorithm used for training
  if re.search(pattern = "_ft_", string = model_name) != None:
      embedding_type_flag = "FastText"
      
  else:
      embedding_type_flag = "Word2Vec"
      
  # check if the model preserved ngrams
  if re.search(pattern = "_no_ngrams_", string = model_name) == None:
      ngrams_flag = True
  
  
  # grab information for the dimensions
  dimensions_flag = int(re.search(pattern = "\d{1,3}", string = re.search(pattern = "d_\d{1,3}_",
                                                                          string = model_name).group(0)).group(0))
  
  # grab information for the window
  window_flag = int(re.search(pattern = "\d{1,3}", string = re.search(pattern = "_w_\d{1,3}", string = model_name).group(0)).group(0))
  
  # grab information for the min word occur
  mwo_flag = int(re.search(pattern = "\d{1,3}", string = re.search(pattern = "_mwo_\d{1,3}", string = model_name).group(0)).group(0))
  
  # grab information for the epochs
  epoch_flag = int(re.search(pattern = "\d{1,4}", string = re.search(pattern = "_e_\d{1,4}", string = model_name).group(0)).group(0))
  
  
  # create a dictionary with all of the information
  title_dict = {"Model Name" : model_name, "Stopwords?" : stopwords_flag, "References Preserved?" : code_refs_flag,
               "Training Algorithm" : embedding_type_flag, "Ngrams?" : ngrams_flag,
               "Dimensions" : dimensions_flag, "Window" : window_flag,
               "Minimum Count Threshold" : mwo_flag, "Number of Training Epochs" : epoch_flag}
  
  return(title_dict)  


##############################################################################
#          |                                                                 #
# SECTION: | ANALOGY FUNCTIONS                                               #
#----------|-----------------------------------------------------------------#
# USE:     | This section defines functions related to analogy evaluations   #
#          |                                                                 #
##############################################################################
################################################################
#           |                                                  #
# FUNCTION: | store_analogy_eval                               #
#-----------|--------------------------------------------------#
# USE:      | stores the results of an evaluation object into  #
#           | a dictionary (for analogies)                     #
#           |                                                  #
################################################################
def store_analogy_eval(analogy_eval_object):
  
  # create a more discriptive dictionary for the values of a word pair evaluation
  eval_dict = {"Overall Analogy Accuracy (%)" : round((analogy_eval_object[0] * 100), 2)}
  
  return eval_dict

################################################################
#           |                                                  #
# FUNCTION: | perform_analogy_evals                            #
#-----------|--------------------------------------------------#
# USE:      | systematically loads embedding models and grabs  #
#           | the word evaluation information. This function   #
#           | returns a list of pandas dataframes              #
#           |                                                  #
################################################################
def perform_analogy_evals(model_path_list, analogy_path_list):
  
  # create an empty storage list to hold the evaluation dataframes
  evaluation_list = []
  
  # loop through the analogy paths
  for analogy_path in analogy_path_list:
    
    # set flag
    improper_analogy_file = False
    
    # create an empty list to store the evaluation dictionaries
    analogy_eval_list = []
    
    # create an empty list to store dictionaries built from model file names
    name_info_dicts = []
    
    for path in model_path_list:
      
      # evaluate the analogies
      loaded_model = gensim.models.Word2Vec.load(path)
      
      try:
        analogy_eval_list.append(store_analogy_eval(loaded_model.wv.evaluate_word_analogies(analogy_path)))
      except TypeError as e:
        del loaded_model
        improper_analogy_file = True
        print(analogy_path)
        print(analogy_path_list.index(analogy_path))
        break
      else:
        # get the name information
        name_info_dicts.append(name_parse(path))
        del loaded_model
        
    if improper_analogy_file == True:
      improper_analogy_file = False
      continue
    
    # update working analogy list
    trunc_analogy_list.append(analogy_path)
    
    # merge the summary evaluation statistics with the name information
    for i in range(len(name_info_dicts)):
        name_info_dicts[i].update(analogy_eval_list[i])
     
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
    
    evaluation_list.append(info_df)
        
  return evaluation_list


##############################################################################
#          |                                                                 #
# SECTION: | PAIR FUNCTIONS                                                  #
#----------|-----------------------------------------------------------------#
# USE:     | This section defines functions related to pair evaluations      #
#          |                                                                 #
##############################################################################
################################################################
#           |                                                  #
# FUNCTION: | store_pair_eval                                  #
#-----------|--------------------------------------------------#
# USE:      | stores the results of an evaluation object into  #
#           | a dictionary (for analogies)                     #
#           |                                                  #
################################################################
def store_pair_eval(pair_eval_object):
  eval_dict = {"Pearson Correlation Coefficient" : round(pair_eval_object[0][0], 4),
               "Pearson Correlation Coefficient P-Value" : round(pair_eval_object[0][1], 4),
               "Spearman Correlation Coefficient" : round(pair_eval_object[1].correlation, 4),
               "Spearman Correlation Coefficient P-Value" : round(pair_eval_object[1].pvalue, 4),
               "Unknown Word Pairs %" : round(pair_eval_object[2], 2)}
  
  return eval_dict

################################################################
#           |                                                  #
# FUNCTION: | perform_pair_evals                               #
#-----------|--------------------------------------------------#
# USE:      | systematically loads embedding models and grabs  #
#           | the word evaluation information. This function   #
#           | returns a list of pandas dataframes              #
#           |                                                  #
################################################################
def perform_pair_evals(model_path_list, pair_path_list):
  
  # create an empty storage list to hold the evaluation dataframes
  evaluation_list = []
  
  # loop through the analogy paths
  for pair_path in pair_path_list:
    
    # create an empty list to store the evaluation dictionaries
    pair_eval_list = []
    
    # create an empty list to store dictionaries built from model file names
    name_info_dicts = []
    
    for path in model_path_list:
      # get the name information
      name_info_dicts.append(name_parse(path))
      
      # evaluate the analogies
      loaded_model = gensim.models.Word2Vec.load(path)
      pair_eval_list.append(store_pair_eval(loaded_model.wv.evaluate_word_pairs(pair_path)))
      del loaded_model
    
    # update working pair list
    trunc_pair_list.append(pair_path)
    
    # merge the summary evaluation statistics with the name information
    for i in range(len(name_info_dicts)):
        name_info_dicts[i].update(pair_eval_list[i])
     
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
    
    evaluation_list.append(info_df)
        
  return evaluation_list


##############################################################################
#          |                                                                 #
# SECTION: | DRIVER                                                          #
#----------|-----------------------------------------------------------------#
# USE:     | This section performs pair and analogy evaluations              #
#          |                                                                 #
##############################################################################
##############################################################################
#          |                                                                 #
#          | GET LIST OF MODELS                                              #
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


##############################################################################
#          |                                                                 #
#          | GET LIST OF ANALOGY FILES                                       #
#          |                                                                 #
##############################################################################
# create an object to hold the analogy file names
analogy_list = []

# create an object to hold the analogy file names that were processed without error
trunc_analogy_list = []

# recursively look in the analogies path for .txt files
for root, dirs, files in os.walk(ANALOGY_PATH):
  for name in files:
    analogy_list.append(os.path.join(root, name))
    
# get rid of any ngram sets if NGRAMS is set to false:
# NOTE: analogy files that have 'ngrams' in the file name will NOT be included
if NGRAMS == False:
  analogy_list = [analogy_name for analogy_name in analogy_list if not re.search("ngram", analogy_name)]
  
  
##############################################################################
#          |                                                                 #
#          | GET LIST OF PAIR FILES                                          #
#          |                                                                 #
##############################################################################
# create an object to hold the analogy file names
pair_list = []

# create an object to hold the pair file names that were processed without error
trunc_pair_list = []

# recursively look in the analogies path for .txt files
for root, dirs, files in os.walk(PAIR_PATH):
  for name in files:
    pair_list.append(os.path.join(root, name))
    
# get rid of any ngram sets if NGRAMS is set to false:
# NOTE: analogy files that have 'ngrams' in the file name will NOT be included
if NGRAMS == False:
  pair_list = [pair_name for pair_name in pair_list if not re.search("ngram", pair_name)]


##############################################################################
#          |                                                                 #
#          | PARALLELIZATION                                                 #
#          |                                                                 #
##############################################################################


  
##############################################################################
#          |                                                                 #
#          | RUN EVALUATIONS                                                 #
#          |                                                                 #
##############################################################################  
# run the validation functions to get analogy accuracy scores
analogy_results = perform_analogy_evals(model_paths, analogy_list)

# run the validation functions to get pair correlations
pair_results = perform_pair_evals(model_paths, pair_list)


##############################################################################
#          |                                                                 #
#          | SAVE EVALUATION DATA AS CSV FILES                               #
#          |                                                                 #
##############################################################################  
## SAVE ANALOGIES ##
for i in range(len(trunc_analogy_list)):
  # get the analogy file name from the analogy path
  analogy_name = (trunc_analogy_list[i].split("/"))[-1]
  
  # strip the extension from the analogy path
  analogy_name = (analogy_name.split("."))[0]
  
  # mark the output file as "AER" for analogy evaluation results
  analogy_name = analogy_name + "_AER.csv"
  
  # complete the output path
  output_path = os.path.join(ANALOGY_OUTPUT_PATH, analogy_name)
  
  # save the evaluation data to a CSV
  analogy_results[i].to_csv(output_path) 


## SAVE PAIRS ## 
for i in range(len(trunc_pair_list)):
  # get the analogy file name from the analogy path
  pair_name = (trunc_pair_list[i].split("/"))[-1]
  
  # strip the extension from the analogy path
  pair_name = (pair_name.split("."))[0]
  
  # mark the output file as "PER" for pair evaluation results
  pair_name = pair_name + "_PER.csv"
  
  # complete the output path
  output_path = os.path.join(PAIR_OUTPUT_PATH, pair_name)
  
  # save the evaluation data to a CSV
  pair_results[i].to_csv(output_path)
