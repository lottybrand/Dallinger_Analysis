# Dallinger_Analysis
These scripts are for analysing the data of an experiment on prestige-biased social learning, run via the "Dallinger" experimental software. 
The experimental scripts can be found at www.github.com/lottybrand/lottysBartlett
The preprint for the corresponding paper can be found here https://psyarxiv.com/mn9t6/ 

The file "data_info" includes an explanation of all of the variables in the data files and the full_data file, and where they come from. 

The file "data_inputting.R" converts the jSon scripts from all the datafiles in the raw_data_files folder into one large database

The file "dallinger_data_cleaning.R" cleans this file and creates new variables, subsets and dataframes to be used in the analysis scripts - culminating in full_data.csv 

The file "analysis_script.R" includes all the analysis models and their corresponding predictions, 
this can be reproduced if dallinger_data_cleaning.R is run first (full_data.csv is the saved dataframe from line 113 of dallinger_data_cleaning.R)
some plots are also produced in this file. 

The folder "results" contains a summary of the main results, some descriptives in "results_prelim" and some detailed results from model4.2, 
this was during confusion over the use of a_bar in these models (see https://twitter.com/LottyBrand/status/1195482057931153408) 

The folder "plots" contains many exploratory plots and some plotting script, although some plots are created in the analysis_script.R

The file "score_check.R" includes code that double-checked our score and copying variables were calculated correctly

The folder "Follow_up_experiment" contains code that explores the within and between topic scores and behaviour, in preparation for our second experiment

The folder "reviewer_responding" includes scripts that helped deal with some reviewer comments 




