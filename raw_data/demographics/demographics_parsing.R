###demographics parsing

setwd("~/Desktop/Postdoc/Lottys_dallinger/Dallinger_Analysis/raw_data/demographics")

# libraries
library(jsonlite)
library(data.table)
library(dplyr)

# 1) First, make a transformation function that works for a single entry
f <- function(json, id){
  # transform json to list
  tmp    <- jsonlite::fromJSON(json)
  # transform list to data.frame
  tmp    <- as.data.frame(tmp)
  # add id
  tmp$id <- id
  # return
  return(tmp)
}


load_file <- function(file, condition) {
  questions <- read.csv(file, stringsAsFactors = FALSE)
  
  # parse the JSON using: https://stackoverflow.com/questions/41988928/how-to-parse-json-in-a-dataframe-column-using-r 
  # using the second option (non-tidyverse): 
  
  
  # 2) apply it via mapply 
  json_dfs <- 
    mapply(f, questions$response, questions$participant_id, SIMPLIFY = FALSE)
  # 3) combine the fragments via rbindlist
  clean_df <- 
    data.table::rbindlist(json_dfs)
  
  #cleanup
  my_data <- clean_df
  clean_df <- NULL
  rm(json_dfs)
  return(my_data)
}

file_names <- c("question_18_b.csv", "question_19_b.csv","question_20_b.csv","question_21_b.csv", "question_24_B.csv", "question_24_A.csv", "question_25_A.csv", "question_26_C.csv")
condition <- c("b","b","b","b","b","a","a","c")
loaded_files <- list()
for (i in 1:length(file_names)) {
  loaded_files[[i]] <- load_file(file_names[i], condition[i])
}

#assign first dataset to questions data_data:
questions_data <- loaded_files[[1]]

#combine all 
for (i in 2:length(loaded_files)) {
  this_table <- loaded_files[[i]]
  questions_data <- bind_rows(questions_data, this_table)
}

questions_data$age <- as.integer(questions_data$age)
questions_data <- questions_data[!questions_data$age=="NA",]
mean(questions_data$age)
table(questions_data$gender)
