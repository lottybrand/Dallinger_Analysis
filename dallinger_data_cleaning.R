#Data cleaning for Dallinger experiment

#setwd("~/Desktop/Postdoc/Lottys_dallinger/Dallinger_Analysis")

# libraries
library(jsonlite)
library(data.table)
library(dplyr)

infos<- read.csv("info.csv", stringsAsFactors = FALSE)
infos <- infos[order(infos$id),]

#trying to parse JSON using: https://stackoverflow.com/questions/41988928/how-to-parse-json-in-a-dataframe-column-using-r 


#Trying second option (non-tidyverse): 

#delete when the origin is the source (we only want participants' data):
infos2<- infos[!infos$origin_id==1,]

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
  mapply(f, infos2$property1, infos2$id, SIMPLIFY = FALSE)
# 3) combine the fragments via rbindlist
clean_df <- 
  data.table::rbindlist(json_dfs)

#use match to add in the other useful variables frmo the info table
clean_df$Contents <- infos2$contents[match(clean_df$id, infos2$id)]
clean_df$Origin <- infos2$origin_id[match(clean_df$id, infos2$id)]

#delete practice round data
clean_df <- clean_df[!clean_df$round==0,]

#a subset for asocial answers only: 
asocialOnly <- clean_df[clean_df$copying=="FALSE",]
#a subset of copying only:
copyOnly <- clean_df[clean_df$copying=="TRUE",]

#create cumulative (asocial) score for each ppt? 
#using ave and cumsum
asocialOnly$c_a_score <- ave(asocialOnly$score, asocialOnly$Origin, FUN=cumsum)

#max scorer per question?
#trying aggregate (bit convoluted, have to then use match, then create isMax)
a <- aggregate(asocialOnly$c_a_score, by = list(asocialOnly$number), max)
asocialOnly$maxScore <- a$x[match(asocialOnly$number, a$Group.1)]

#trying to then say which ppt (origin) was the highest scoring, BUT
#don't know what to do about ties, this keeps ties right now. 
#should probably write a function....
asocialOnly$isMax <- ifelse((asocialOnly$maxScore == asocialOnly$c_a_score),asocialOnly$Origin,NA)
topScorers <- asocialOnly[!is.na(asocialOnly$isMax),]
topScorers <- subset(topScorers, select =c("number","isMax"))

nodeIDs <- (unique(asocialOnly$Origin))
copyOnlyContents <- copyOnly[!copyOnly$Contents%in%nodeIDs,]
copyOnlyIds <- copyOnly[copyOnly$Contents%in%nodeIDs,]

#steve's code:
#make a new variable:
copyOnlyIds$topCopy <- rep(NA, length(copyOnlyIds$Contents))


# Check the copyOnlyIds ' contents column to see if it's in the topscorers$isMax column, For That Number
# Give a 1 to topcopy if the target is on the list

#copyOnlyIds$topCopy <- if((copyOnlyIds[(copyOnlyIds$Contents %in% topScorers$isMax),]$number), 1, 0) 
#try to merge this with above to make an ifelse inside the for loop. 
numbers <- unique(topScorers$number)
for (n in numbers) {
  copyOnlyIds$topCopy[topScorers$number == n] <- copyOnlyIds$Contents[topScorers$number == n] %in% topScorers$isMax[topScorers$number == n]
}


