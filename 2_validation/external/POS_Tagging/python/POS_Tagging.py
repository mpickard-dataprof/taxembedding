########################################################################################
#            |                                                                         #
# FILE:      | POS_Tagging.py                                                          #
#------------|-------------------------------------------------------------------------#
# USE:       | This file implements the Keras and Tensorflow API to construct a POS    #
#            | tagging model from gensim word embeddings.                              #
#------------|-------------------------------------------------------------------------#
# AUTHOR(S): | Alexander Wold (Z1817662)                                               #
#------------|-------------------------------------------------------------------------#
# DATE:      | October 19, 2020                                                        #
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
CORPUS_PATH = "/home/mpickard/Projects/ustax/data/Train_and_Test/irc26_corpus.txt"
MODEL_PATH = "/home/mpickard/Projects/ustax/data/embeddings/nosw_norefs_w2v_ngrams_d_128_w_10_mwo_10_e_10"


##############################################################################
#          |                                                                 #
# SECTION: | PACKAGES                                                        #
#----------|-----------------------------------------------------------------#
# USE:     | loads the required packages                                     #
#          |                                                                 #
##############################################################################
import gensim
import keras
import tensorflow as tf
import numpy as np
import collections
import nltk
import sklearn
import statistics
from sklearn.model_selection import train_test_split
from keras.preprocessing.sequence import pad_sequences
from keras.models import Sequential
from keras.layers import Dense, LSTM, InputLayer, Bidirectional, TimeDistributed, Embedding, Activation, Flatten
from keras.optimizers import Adam
from keras.callbacks import EarlyStopping


##############################################################################
#          |                                                                 #
# SECTION: | FUNCTIONS                                                       #
#----------|-----------------------------------------------------------------#
# USE:     | provides fundamental functionality for the script               #
#          |                                                                 #
##############################################################################
################################################################
#           |                                                  #
# FUNCTION: | get_tag_vocabulary                               #
#-----------|--------------------------------------------------#
# USE:      | this function builds a dictionary mapping of POS #
#           | tags to unique ids                               #
#           |                                                  #
################################################################
# define a function to get a dictionary mapping of POS-tags to unique ids
def get_tag_vocabulary(tagged_sentences):

  # create an empty dictionary
  tag2id = {}
  
  # iterate through all tagged sentences
  for sentence in tagged_sentences:
  
    # iterate through all tagged words
    for tag in sentence:
    
      # put all unique tags in the dictionary giving them the newest key (which is the latest length of the dict)
      tag2id.setdefault(tag, len(tag2id))
  
  # return the dictionary mapping
  return tag2id


################################################################
#           |                                                  #
# FUNCTION: | add_new_word                                     #
#-----------|--------------------------------------------------#
# USE:      | this function adds embeddings for unknown words  #
#           | to the loaded embedding model                    #
#           |                                                  #
################################################################
# this function adds embeddings for unknown words to the loaded embedding model
def add_new_word(new_word, new_vector, new_index, embedding_matrix, word_dict, addAsTag = False, tag_dict = [], new_tag_index = 0):
  
  # insert the vector before the given index, along the rows (axis 0)
  embedding_matrix = np.insert(embedding_matrix, [new_index], [new_vector], axis = 0)
  
  # update the indexes of words that follow the new word
  word_dict = {word: (index + 1) if index >= new_index else index for word, index in word_dict.items()}
  
  # add the new word to the word id dictionary
  word_dict[new_word] = new_index
  
  
  # if the user wants to add the word as a tag as well, let them
  if addAsTag == True:
  
    # add the word to the tag dictionary
    tag_dict = {tag: (index + 1) if index >= new_tag_index else index for tag, index in tag_dict.items()}
    
    tag_dict[new_word] = new_tag_index
  
  
  # return the updated embedding matrix and word id dictionary (also return the tag dictionary if applicable)
  if addAsTag == False:
    return embedding_matrix, word_dict
    
  else:
    return embedding_matrix, word_dict, tag_dict


################################################################
#           |                                                  #
# FUNCTION: | get_id_data                                      #
#-----------|--------------------------------------------------#
# USE:      | this function replaces all words and tags with   #
#           | their corresponding ids - this function          #
#           | separates the words from the tags                #
#           |                                                  #
################################################################
# define a function to replace all words and tags with their corresponding ids - this function separates the words from the tags
def get_id_data(sentences, tag_sequences, word_dict, tag_dict):

  # define seperate, empty lists to hold the word and tag ids
  word_ids = []
  tag_ids = []
  
  # define a variable to hold the ratio of words we don't have a representation for
  unk_count = 0
  
  # define a variable to hold the total number of words
  wor_count = 0
  
  
  
  # loop through the sentences
  for sentence in sentences:
  
    # define loop variables
    loop_word_ids = []
  
    # loop through each word in each sentence
    for word in sentence:
    
      # increment the word counter
      wor_count += 1
      
      # if the word belongs to the dictionary, get its unique id
      if word in word_dict:
        loop_word_ids.append(word_dict.get(word))
        
      # if the word does not belong to the dictionary, assign the unknown id to it
      else:
        loop_word_ids.append(UNK_INDEX)
        unk_count += 1
        
    # append the loop words to the word ids master list
    word_ids.append(loop_word_ids)
  
  
  
  # loop through the tag sequences   
  for tag_sequence in tag_sequences:
  
    # define loop variables
    loop_tag_ids = []
  
    # loop through the tags in each tag sequence
    for tag in tag_sequence:
    
      # assign each tag its id value
      loop_tag_ids.append(int(tag_dict.get(tag)))
      
    # append the loop tags to the tag ids master list
    tag_ids.append(loop_tag_ids)
  
  # print the ratio of unknown words
  print("Percentage of unknown words: ", (unk_count / wor_count))
  
  # return the lists of ids as numpy arrays
  return word_ids, tag_ids


################################################################
#           |                                                  #
# FUNCTION: | to_categorical                                   #
#-----------|--------------------------------------------------#
# USE:      | this function converts sequences of tags into    #
#           | one-hot encoded tags for dense layers            #
#           |                                                  #
################################################################
# define a function to convert sequences of tags into one-hot encoded tags for dense layers
def to_categorical(tag_sequences, number_of_categories):

  # define an empty array to hold category sequences
  category_sequences = []
  
  # loop through the tag sequences
  for tag_sequence in tag_sequences:
    
    # define loop variables
    loop_categories = []
    
    # loop through each tag
    for tag in tag_sequence:
    
      # append a vector of zeros the length of the number of possible categories
      loop_categories.append(np.zeros(number_of_categories))
      
      # get the most recently appended category vector and put a one in the position of the tag
      loop_categories[-1][tag] = 1
      
    # append each list of one-hot encoded tags to the category sequences list
    category_sequences.append(loop_categories)
    
  # return the array of one-hot encoded tag vectors
  return np.array(category_sequences)


################################################################
#           |                                                  #
# FUNCTION: | define_pos_model                                 #
#-----------|--------------------------------------------------#
# USE:      | this function defines a keras model that uses an #
#           | embedding layer derived from the gensim results  #
#           |                                                  #
################################################################
def define_pos_model(embedding_matrix, word_count, class_count):
  # define the model
  pos_model = Sequential()
  pos_model.add(InputLayer(input_shape = (MED_LENGTH, )))
  pos_model.add(Embedding(word_count, 128, weights = [embedding_matrix]))
  pos_model.add(Bidirectional(LSTM(256, return_sequences = True)))
  pos_model.add(TimeDistributed(Dense(class_count)))
  pos_model.add(Activation("sigmoid"))

  # rules for compiling the model
  pos_model.compile(loss = "categorical_crossentropy",
                    optimizer = Adam(0.001),
                    metrics = ["accuracy"])
  
  # return the compiled model          
  return pos_model
  

################################################################
#           |                                                  #
# FUNCTION: | predict_to_categorical                           #
#-----------|--------------------------------------------------#
# USE:      | this function translates predictions into        #
#           | categorical classes that are easier to read      #
#           |                                                  #
################################################################
# define a function to translate predictions into categorical classes that are easier to read
def predict_to_categorical(token_sequences, tag_dict):

  # convert the tag dictionary into an index
  index = {i: t for t, i in tag_dict.items()}

  # define an empty array to hold sequences of translated categories
  categorical_sequences = []
  
  # loop through each sequence in the list of token sequences
  for token_sequence in token_sequences:
  
    # define loop variables
    loop_sequence = []
    
    # loop through the tokens in each token sequence
    for token in token_sequence:
    
      # find the most probable tag and append it to the sentence tag list
      loop_sequence.append(index[np.argmax(token)])
      
    # append each sentence's list of most probable tags
    categorical_sequences.append(loop_sequence)
    
  # return the list of most likely tags
  return categorical_sequences


##############################################################################
#          |                                                                 #
# SECTION: | DRIVER                                                          #
#----------|-----------------------------------------------------------------#
# USE:     | generates a tensorflow/keras model from the provided gensim     #
#          | word embedding and evaluates its performance in regards to      #
#          | parts of speech tagging                                         #
#          |                                                                 #
##############################################################################
# open the training and testing data
corpus_file = open(CORPUS_PATH)

# read in the text data
text_cor = corpus_file.read()

# close the file connections
corpus_file.close()

# load the word embedding
loaded_embedding = gensim.models.Word2Vec.load(MODEL_PATH)

# get the embedding vectors
word_vectors = loaded_embedding.wv

# tokenize the text corpus by sentences
sentence_tokens = nltk.sent_tokenize(text_cor)

# define two empty arrays: 1 to hold a list of sentences, and another to hold a list of sentence tags
sentence_words = []
sentence_tags = []

# loop through all of the tagged sentences
for sentence in sentence_tokens:

  # define two empty arrays: 1 to hold a list of words found in the sentence, and another to hold a list of the tags applied to each word
  loop_words = []
  loop_tags = []

  # tokenize the sentences into words
  tag_sentence = nltk.word_tokenize(sentence)
  
  # pass in the tokenized words into the POS tagger
  tag_pos = nltk.pos_tag(tag_sentence)
  
  # loop through all of the tag pairs captured by the POS tagger
  for word, tag in tag_pos:
  
    # append the word to the word array for this particular sentence
    loop_words.append(word)
    
    # append the tag to the tag array for this particular sentence
    loop_tags.append(tag)
    
  # append the list of words and tags found in this sentence to the master lists of sentences and tags
  sentence_words.append(np.array(loop_words))
  sentence_tags.append(np.array(loop_tags))
  
# split the text data into training and testing data
(train_sentences, test_sentences, train_tags, test_tags) = train_test_split(sentence_words, sentence_tags,
                                                                            test_size = .2)
                                                                            
# create a dictionary of words and their ids from the word embedding
word2id = {k: v.index for k, v in word_vectors.vocab.items()}

# create a dictionary of tags and their ids
tag2id = get_tag_vocabulary(sentence_tags)

# create baseline tag for padding tokens
PAD_INDEX = 0
PAD_TOKEN = "PAD"

# create baseline tag info for words we don't have an embedding for
UNK_INDEX = 1
UNK_TOKEN = "UNK"

# get the embedding matrix
embedding_matrix = word_vectors.vectors

# assign the padding vector as zero
pad_vector = 0

# assign the unknown vector as the mean of all embedded vectors (along axis zero: the rows)
unk_vector = embedding_matrix.mean(0)

print(tag2id)

# add the new unknown word and update the word id dictionary and embedding matrix
embedding_matrix, word2id, tag2id = add_new_word(PAD_TOKEN, pad_vector, PAD_INDEX, embedding_matrix, word2id,
                                         addAsTag = True, tag_dict = tag2id, new_tag_index = 0)
embedding_matrix, word2id = add_new_word(UNK_TOKEN, unk_vector, UNK_INDEX, embedding_matrix, word2id)

print(tag2id)

# replace
word_id_train, tag_id_train = get_id_data(train_sentences, train_tags, word2id, tag2id)
word_id_test, tag_id_test = get_id_data(test_sentences, test_tags, word2id, tag2id)

# calculate the maximum length of all sequences in the training data set
MAX_LENGTH = len(max(word_id_train, key = len))

# calculate the median length of all sequences in the training data set
sentence_lengths = []
for sentence in word_id_train:
  sentence_lengths.append(len(sentence))

MED_LENGTH = int(statistics.median(sentence_lengths))

# pad the training and testing sequences
word_id_train = pad_sequences(sequences = word_id_train, maxlen = MED_LENGTH, padding = "post", truncating = "post",
                              value = float(pad_vector))
tag_id_train = pad_sequences(sequences = tag_id_train, maxlen = MED_LENGTH, padding = "post", truncating = "post",
                             value = float(pad_vector))
word_id_test = pad_sequences(sequences = word_id_test, maxlen = MED_LENGTH, padding = "post", truncating = "post",
                             value = float(pad_vector))
tag_id_test = pad_sequences(sequences = tag_id_test, maxlen = MED_LENGTH, padding = "post", truncating = "post",
                            value = float(pad_vector))


# use the function defined above to create a one-hot encoding for the tag categories
cat_tag_id_train = to_categorical(tag_id_train, len(tag2id))
print(cat_tag_id_train)

# use the function define above to compile and summarize a word embedding model        
pos_model = define_pos_model(embedding_matrix, len(word2id), len(tag2id))
pos_model.summary()

# define a callback function
early_stopping = EarlyStopping(monitor = "accuracy", mode = "max", verbose = 1, patience = 50)

# fit the model
pos_model.fit(word_id_train, cat_tag_id_train, batch_size = 128, epochs = 200, verbose = 1, callbacks = [early_stopping])

# encode tag categories for the testing set
cat_tag_id_test = to_categorical(tag_id_test, len(tag2id))

# evaluate the model based on the test data
scores = pos_model.evaluate(word_id_test, cat_tag_id_test)
print(scores)

# define test samples
samples = ["Section 25 permits States and political subdivisions to elect to issue mortgage credit certificates in lieu of qualified mortgage bonds.".split(), "An individual who holds a qualified mortgage credit certificate (as defined in § 1.25-3T) is entitled to a credit against his Federal income taxes.".split()]

# convert the words into tags
sample_ids = get_id_data(samples, [], word2id, tag2id)[0]

# pad the sequence
padded_sample_ids = pad_sequences(sequences = sample_ids, maxlen = MED_LENGTH, padding = "post", value = float(pad_vector))


# get a the models predictions for each test sentence
predictions = pos_model.predict(padded_sample_ids)

# convert the predictions into categorical data
cat_predictions = predict_to_categorical(predictions, tag2id)

print(cat_predictions)
