
#Data cleaning for Dallinger experiment

source('data_inputting.R')

##### Merge data from all conditions

full_data <- bind_rows(my_data,my_data_a,my_data_c)

#####
#####
##### NEED TO GO BACK OVER THIS to account for duplicate Origin_ids between networks/conditions #####
#####
#####


# a subset for asocial answers only: 
asocialOnly <- my_data[my_data$copying=="FALSE",]
# a subset of copying only:
copyOnly <- my_data[my_data$copying=="TRUE",]

# create cumulative (asocial) score for each ppt? 
# using ave and cumsum
asocialOnly$c_a_score <- ave(asocialOnly$score, asocialOnly$Origin, FUN=cumsum)

# max scorer per question?
# trying aggregate (bit convoluted, have to then use match, then create isMax)
a <- aggregate(asocialOnly$c_a_score, by = list(asocialOnly$number), max)
asocialOnly$maxScore <- a$x[match(asocialOnly$number, a$Group.1)]
#cleanup
rm(a)

# trying to then say which ppt (origin) was the highest scoring, BUT
# don't know what to do about ties, this keeps ties right now. 
# should probably write a function....
asocialOnly$isMax <- ifelse((asocialOnly$maxScore == asocialOnly$c_a_score),asocialOnly$Origin,NA)
topScorers <- asocialOnly[!is.na(asocialOnly$isMax),]
topScorers <- subset(topScorers, select =c("number","isMax"))

nodeIDs <- (unique(asocialOnly$Origin))
copyOnlyContents <- copyOnly[!copyOnly$Contents%in%nodeIDs,]
copyOnlyIds <- copyOnly[copyOnly$Contents%in%nodeIDs,]

# make a new variable:
copyOnlyIds$topCopy <- rep(NA, length(copyOnlyIds$Contents))

# Check the copyOnlyIds' contents column to see if it's in the topscorers$isMax column, For That Number
# Give a 1 to topcopy if the target is on the list

# copyOnlyIds$topCopy <- if((copyOnlyIds[(copyOnlyIds$Contents %in% topScorers$isMax),]$number), 1, 0) 
# try to merge the following with above to make an ifelse inside the for loop. 
numbers <- unique(topScorers$number)
for (n in numbers) {
  copyOnlyIds$topCopy[topScorers$number == n] <- copyOnlyIds$Contents[topScorers$number == n] %in% topScorers$isMax[topScorers$number == n]
}
copyOnlyIds$topCopy <- copyOnlyIds$topCopy*1

#subset for seeing score info
scoreChoice <-copyOnlyIds[copyOnlyIds$round==1,]

#in full dataset it will be:
#scoreChoice <-copyOnlyIds[(copyOnlyIds$round==1 & copyOnlyIds$Condition!="A") | (copyOnlyIds$info_chosen="Total Score in Round 1"),]

#get ncopies for each node

ncopies <- tapply(copyOnlyIds$copying, list(copyOnlyIds$Contents),sum)
ncopies <- as.data.frame(ncopies)
setDT(ncopies, keep.rownames = "copied")
#what if there are more than one maximums?
maxCopied <- which.max(ncopies$ncopies)

#subset for seeing prestige info:
prestigeChoice <- copyOnlyIds[copyOnlyIds$info_chosen =="Times chosen in Round 1",]
prestigeChoice$presCopy <- ifelse((prestigeChoice$Contents %in% maxCopied),1,0)

#need to account for if the maxCopied was available to copy then (ie wasn't in Origin)
#THE FOLLOWING DOES NOT WORK, it converts Contents column to 2s:
#prestigeChoice$presCopy <- if(prestigeChoice$Origin!=maxCopied) {
#  ifelse((prestigeChoice$Contents=maxCopied),1,0)
#}


# for info_chosen analyses
infoChoice <- copyOnlyContents[copyOnlyContents$round==2,]
infoChoice$Condition <- "B"
