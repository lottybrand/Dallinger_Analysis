
#### inputting the data from Dallinger per Condition ####
#### should really use a loop to do all of this for each condition.... 

#setwd("~/Desktop/Postdoc/Lottys_dallinger/Dallinger_Analysis")

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
  infos <- read.csv(file, stringsAsFactors = FALSE)
  #infos <- infos[order(infos$id),]

  # parse the JSON using: https://stackoverflow.com/questions/41988928/how-to-parse-json-in-a-dataframe-column-using-r 
  # using the second option (non-tidyverse): 

  # delete when the origin is the source (we only want participants' data):
  infos<- infos[infos$type=="lotty_info" & infos$property1 != "",]

  # 2) apply it via mapply 
  json_dfs <- 
    mapply(f, infos$property1, infos$id, SIMPLIFY = FALSE)
  # 3) combine the fragments via rbindlist
  clean_df <- 
    data.table::rbindlist(json_dfs)

  #cleanup
  my_data <- clean_df
  clean_df <- NULL
  rm(json_dfs)

  # use match to add in the other useful variables from the info table
  my_data$Contents <- infos$contents[match(my_data$id, infos$id)]
  my_data$Origin <- infos$origin_id[match(my_data$id, infos$id)]
  my_data$Network <- infos$network_id[match(my_data$id, infos$id)]

  # delete practice round data
  my_data <- my_data[!my_data$round==0,]
  my_data$condition <- rep(condition, nrow(my_data))
  return(my_data)
}

file_names <- c("info_18_b.csv", "info_19_b.csv","info_20_b.csv","info_21_b.csv", "info_24_B.csv", "info_24_A.csv", "info_25_A.csv", "info_26_C.csv")
condition <- c("b","b","b","b","b","a","a","c")
loaded_files <- list()
for (i in 1:length(file_names)) {
  loaded_files[[i]] <- load_file(file_names[i], condition[i])
}


# Condition, n_copies, asoc_Score etc are all in the node files..... UGH
#"info_20_b.csv","info_21_b.csv", "info_24_B.csv", "info_24_A.csv", "info_25_A.csv", "info_26_C.csv"