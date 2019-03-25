#### inputting the data from Dallinger per Condition ####
#setwd("~/Desktop/Postdoc/Lottys_dallinger/Dallinger_Analysis")

# libraries
library(jsonlite)
library(data.table)
library(dplyr)

####
#### CONDITION B First (this was the pilot data) ####
####
####
####

infos<- read.csv("info.csv", stringsAsFactors = FALSE)
infos <- infos[order(infos$id),]

# trying to parse JSON using: https://stackoverflow.com/questions/41988928/how-to-parse-json-in-a-dataframe-column-using-r 
# second option (non-tidyverse): 

# delete when the origin is the source (we only want participants' data):
infos<- infos[infos$type=="lotty_info",]

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

# use match to add in the other useful variables frmo the info table
my_data$Contents <- infos$contents[match(my_data$id, infos$id)]
my_data$Origin <- infos$origin_id[match(my_data$id, infos$id)]
my_data$Network <- infos$network_id[match(my_data$id, infos$id)]

# delete practice round data
my_data <- my_data[!my_data$round==0,]

# add Condition Column: 
my_data$condition <- "B"

####
#### CONDITION A Next ####
####
####
####

infosA<- read.csv("infoA_twoNetwork.csv", stringsAsFactors = FALSE)
infosA <- infosA[order(infosA$id),]

# trying to parse JSON using: https://stackoverflow.com/questions/41988928/how-to-parse-json-in-a-dataframe-column-using-r 
# second option (non-tidyverse): 

# delete when the origin is the source (we only want participants' data):
infosA<- infosA[infosA$type=="lotty_info",]

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
# 2) apply it via mapply 
json_dfs <- 
  mapply(f, infosA$property1, infosA$id, SIMPLIFY = FALSE)
# 3) combine the fragments via rbindlist
my_data_a <- 
  data.table::rbindlist(json_dfs)

#cleanup
rm(json_dfs)

# use match to add in the other useful variables frmo the info table
my_data_a$Contents <- infosA$contents[match(my_data_a$id, infosA$id)]
my_data_a$Origin <- infosA$origin_id[match(my_data_a$id, infosA$id)]
my_data_a$Network <- infosA$network_id[match(my_data_a$id, infosA$id)]
# delete practice round data
my_data_a <- my_data_a[!my_data_a$round==0,]

# add Condition Column: 
my_data_a$condition <- "A"

####
####
#### NOW FOR CONDITION C ####
####
####

infosC<- read.csv("infoC_twoNetwork.csv", stringsAsFactors = FALSE)
infosC <- infosC[order(infosC$id),]

# trying to parse JSON using: https://stackoverflow.com/questions/41988928/how-to-parse-json-in-a-dataframe-column-using-r 
# second option (non-tidyverse): 

# delete when the origin is the source (we only want participants' data):
infosC<- infosC[infosC$type=="lotty_info",]

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
# 2) apply it via mapply 
json_dfs <- 
  mapply(f, infosC$property1, infosC$id, SIMPLIFY = FALSE)
# 3) combine the fragments via rbindlist
my_data_c <- 
  data.table::rbindlist(json_dfs)

#cleanup
rm(json_dfs)

# use match to add in the other useful variables frmo the info table
my_data_c$Contents <- infosC$contents[match(my_data_c$id, infosC$id)]
my_data_c$Origin <- infosC$origin_id[match(my_data_c$id, infosC$id)]
my_data_c$Network <- infosC$network_id[match(my_data_c$id, infosC$id)]

# delete practice round data
my_data_c <- my_data_c[!my_data_c$round==0,]

# add Condition Column: 
my_data_c$condition <- "C"

