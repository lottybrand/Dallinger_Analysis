
#Data prep for Dallinger analysis

source('data_inputting.R')

##### Merge data from all conditions after data_unputting.R
full_data <- bind_rows(my_data,my_data_a,my_data_c)

#because the info 'Origin' is repeated each time we run it, we need to renumber participants based on their condition and network
#start by ordering according to condition, network and number(question)
full_data$number <- as.integer(full_data$number)
full_data <- full_data[order(condition,Network,number),]

#because we ran the two networks within condition A, their Origin Ids are already independent, so only need to account for 
#condition right now, but in full run might need to account for network too:

# This is getting close but it's not treating new conditions as new sets...
#full_data$num <- with(full_data, ave(Origin, condition, FUN=function(x) match(x, unique(Origin))))

#this just starts at 1 and keeps iterating up. some combo of this and above..?
#full_data$num <- ave(full_data$Origin, full_data$condition, FUN = seq_along)

# don't know what this thinks it's doing: 
#for (i in unique(full_data$Origin)) full_data$num[full_data$Origin == i] <- seq_len(sum(full_data$Origin == i))

#tom houslay's doesn't work either:
#df_index <- full_data %>% distinct(Origin, condition) %>% mutate(num = row_number())
#full_data <- left_join(full_data, df_index)

#time for a bodge job for now:
full_data$num <- with(full_data, ave(Origin, condition, FUN=function(x) match(x, unique(x))))
pptsA <- length(unique(my_data_a$Origin))

full_data$num <- ifelse(full_data$condition=="B", full_data$num+pptsA,full_data$num)

pptsB <- length(unique(my_data$Origin))
full_data$num <- ifelse(full_data$condition=="C", full_data$num+pptsA+pptsB,full_data$num)

colnames(full_data)[12] <- "ppt"

# manually reindex network too, as in full run they will be individual (at least per condition):
full_data$group <- ifelse((full_data$condition=="B"),3,
                          ifelse((full_data$condition=="C" & full_data$Network==1),4,
                            ifelse((full_data$condition=="C" & full_data$Network==2), 5, full_data$Network)))


#####
##### Need to do the same for Contents when it is a copying decision.... how do we match the content ID with the Origin id throughout...?
#####

#####
##### Subsets for different analyses 
#####

#####
##### Subset for ASOCIAL ONLY : 
#####

asocialOnly <- full_data[full_data$copying=="FALSE",]
#asocialOnly <- my_data[my_data$copying=="FALSE",]

# create cumulative (asocial) score for each ppt? need to check this for full_data 
# using ave and cumsum
# will this work for the new ppt column..?!?!
asocialOnly$c_a_score <- ave(asocialOnly$score, asocialOnly$ppt, FUN=cumsum)

#asocialOnly$c_a_score <- ave(asocialOnly$score, asocialOnly$Origin, FUN=cumsum)

# max scorer per question, per group 
# trying aggregate (bit convoluted, have to then use match, then create isMax)
a <- aggregate(asocialOnly$c_a_score, by = list(asocialOnly$number, asocialOnly$group), max)

#need to match according to both groups:
asocialOnly <- merge(asocialOnly, a, by.x=c("number", "group"), by.y=c("Group.1", "Group.2"), all.x=TRUE, all.y=FALSE)
asocialOnly <- asocialOnly[order(condition,Network,number),]
names(asocialOnly)[names(asocialOnly) == 'x'] <- 'maxScore'

#cleanup
rm(a)

# Make a list ("topScorers") of which ppt (origin) was the highest scoring per question, BUT
# don't know what to do about ties, this keeps ties right now. 
asocialOnly$isMax <- ifelse((asocialOnly$maxScore == asocialOnly$c_a_score),asocialOnly$ppt,NA)
topScorers <- asocialOnly[!is.na(asocialOnly$isMax),]
topScorers <- subset(topScorers, select =c("number","isMax","group","condition"))

#####
##### SUBSET OF COPYING ONLY:
#####

copyOnly <-full_data[full_data$copying=="TRUE",]

#copyOnly <- my_data[my_data$copying=="TRUE",]

#splitting the copying into who they copied and what they copied:
#nodeIDs <- (unique(asocialOnly$Origin))

# need to figure out how to link the copied nodes to their ppt ids......
# first make reference dataframe:
node_index <- subset(full_data, select =c(Origin,ppt,group,condition))
node_index <-unique(node_index)

#just select the Ids:
nodeIDS <- (unique(full_data$Origin))
nodeIDS
#copyOnlyContents <- copyOnly[!copyOnly$Contents%in%nodeIDS,]
copyOnlyIds <- copyOnly[copyOnly$Contents%in%nodeIDS,]

for (n in copyOnlyIds$Contents) {
  copyOnlyIds$copied_node <- node_index$ppt[match(copyOnlyIds$Contents, node_index$Origin)]
}

##### Make variable for if they copied highest scorer:
# make a new variable:
copyOnlyIds$topCopy <- rep(NA, length(copyOnlyIds$Contents))

# Check the copyOnlyIds' contents column to see if it's in the topscorers$isMax column, For That Number & Group
# Give a 1 to topcopy if the target is on the list

# copyOnlyIds$topCopy <- if((copyOnlyIds[(copyOnlyIds$Contents %in% topScorers$isMax),]$number), 1, 0) 
# try to merge the following with above to make an ifelse inside the for loop
# also want to fix this to be "out of those available to copy at the time, was it the maximum. As need to account for
# if the person copying is the maximum themself etc
# so just need to add in some if statements into this for loop I guess.... 

numbers <- unique(topScorers$number)
groups <- unique(topScorers$group)

for (n in numbers) 
  {
  for (g in groups) 
    {
  copyOnlyIds$topCopy[topScorers$number == n & topScorers$group ==g] <- copyOnlyIds$copied_node[topScorers$number == n & topScorers$group == g] %in% topScorers$isMax[topScorers$number == n & topScorers$group ==g]
  }
}

copyOnlyIds$topCopy <- copyOnlyIds$topCopy*1


#####
##### Subset for seeing score info (Prediction 1)
#####

#scoreChoice <-copyOnlyIds[copyOnlyIds$round==1,]

#in full dataset it will be:
scoreChoice <- copyOnlyIds[((!copyOnlyIds$condition=="A")&(copyOnlyIds$round==1))|(copyOnlyIds$info_chosen=="Total Score in Round 1"),]                                     

#####
#####
##### Subset for seeing prestige info (Prediction 2)
#####

#get ncopies for each node
ncopies <- tapply(copyOnlyIds$copying, list(copyOnlyIds$Contents),sum)
ncopies <- as.data.frame(ncopies)
setDT(ncopies, keep.rownames = "copied")
#what if there are more than one maximums?
maxCopied <- which.max(ncopies$ncopies)

prestigeChoice <- copyOnlyIds[copyOnlyIds$info_chosen =="Times chosen in Round 1",]
prestigeChoice$presCopy <- ifelse((prestigeChoice$Contents %in% maxCopied),1,0)

#need to account for if the maxCopied was available to copy then (ie wasn't in Origin)
#THE FOLLOWING DOES NOT WORK, it converts Contents column to 2s:
#prestigeChoice$presCopy <- if(prestigeChoice$Origin!=maxCopied) {
#  ifelse((prestigeChoice$Contents=maxCopied),1,0)
#}


#####
##### Subset of info chosen for Prediction 3:
#####


infoChosen <- copyOnlyContents[copyOnlyContents$round==2,]
infoChosen$chosePrestige <- ifelse(infoChosen$info_chosen=="Times chosen in Round 1",1,0)

# make Condition B the baseline:

infoChosen$CondA <- ifelse(infoChosen$condition =="A", 1, 0)
infoChosen$CondC <- ifelse(infoChosen$condition =="C", 1, 0)






