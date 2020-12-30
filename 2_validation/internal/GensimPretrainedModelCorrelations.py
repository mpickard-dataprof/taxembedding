# set the files of word pairs and analogies to evaluate the pretrained embedding off of
PAIRS_PATH = "/home/alex/Projects/taxembed/data/output/tallied/pairs/unigram-rel"
ANALOGIES_PATH = "/home/alex/Projects/taxembed/data/output/tallied/analogies/analogies-unigrams"
OUTPUT_FILE = "/home/alex/Projects/taxembed/data/output/validations/pretrained_correlations_customunigram_relatedness"

# import packages
import gensim
import csv
import gensim.downloader as gd

# load the google news 300 pretrained model
premodel = gd.load("word2vec-google-news-300")


################################################################
#           |                                                  #
# FUNCTION: | load_evals                                       #
#-----------|--------------------------------------------------#
# USE:      | systematically loads embedding models and grabs  #
#           | the word evaluation information.                 #
#           |                                                  #
################################################################
def load_evals(loaded_model):
    
    # create an empty list to store the evaluation dictionaries
    eval_list = []
    
    # evaluate the embedding and store the results in a dictionary
    eval_list.append(store_pair_eval(loaded_model.evaluate_word_pairs(PAIRS_PATH), loaded_model.evaluate_word_analogies(ANALOGIES_PATH)))
        
    return eval_list
    
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
   
# evaluate the pretrained model - store the results in a dictionary
summary_pretrained = load_evals(premodel)
print(summary_pretrained)

# store the evaluation
summary_dict = summary_pretrained[0]
field_names = summary_dict.keys()

with open(OUTPUT_FILE, 'w') as csvfile:
    writer = csv.DictWriter(csvfile, fieldnames = field_names)
    writer.writeheader()
    writer.writerow(summary_dict)
