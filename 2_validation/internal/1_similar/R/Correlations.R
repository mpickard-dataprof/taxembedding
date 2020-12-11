########################################################################################
#            |                                                                         #
# FILE:      | Correlations.R                                                          #
#------------|-------------------------------------------------------------------------#
# USE:       | This file calculates the correlation data for each word embedding       #
#            | with in relation to human scored word-pairs or analogies                #
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
#          | the behaviour of the script                                     #
#          |                                                                 #
##############################################################################
PYTHON_CORRELATION_SCRIPT <- "/home/mpickard/Projects/ustax/Intrinsic_Evaluators/Word_Pairs_and_Analogies/Correlations.py"
PYTHON_CSV_LOCATION <- "/home/mpickard/Projects/ustax/data/output_data/tst_script_corr.csv"


##############################################################################
#          |                                                                 #
# SECTION: | PACKAGES                                                        #
#----------|-----------------------------------------------------------------#
# USE:     | load the necessary packages for implementation                  #
#          |                                                                 #
##############################################################################
library(reticulate)


##############################################################################
#          |                                                                 #
# SECTION: | DRIVER                                                          #
#----------|-----------------------------------------------------------------#
# USE:     | call the correlations.py script to calculate the correlation    #
#          | values and prints that information to the console               #
#          |                                                                 #
##############################################################################
py_run_file(PYTHON_CORRELATION_SCRIPT)
model_info_df <- read.csv(PYTHON_CSV_LOCATION, header = TRUE)
print(model_info_df)

