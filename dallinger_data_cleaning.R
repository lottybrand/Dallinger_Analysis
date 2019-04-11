
# Data prep for Dallinger analysis

source('data_inputting.R')

##### Merge data from all conditions after data_unputting.R
#### this shouldn't be needed anymore 
full_data <- bind_rows(my_data,my_data_a,my_data_c)

#because the info 'Origin' is repeated each time we run it, we need to renumber participants based on their condition and network
#start by ordering according to condition, network and number(question)
full_data$number <- as.integer(full_data$number)
full_data <- full_data[order(condition,Network,number),]

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
full_data$ppt <- with(full_data, ave(Origin, condition, FUN=function(x) match(x, unique(x))))
pptsA <- length(unique(my_data_a$Origin))

full_data$ppt <- ifelse(full_data$condition=="B", full_data$ppt+pptsA,full_data$ppt)

pptsB <- length(unique(my_data$Origin))
full_data$ppt <- ifelse(full_data$condition=="C", full_data$ppt+pptsA+pptsB,full_data$ppt)

# manually reindex network too for now, as in full run they will be individual (at least per condition)
# need to automate this for the real thing, maybe like the above?
full_data$group <- ifelse((full_data$condition=="B"),3,
                          ifelse((full_data$condition=="C" & full_data$Network==1),4,
                            ifelse((full_data$condition=="C" & full_data$Network==2), 5, full_data$Network)))

#####
##### Subsets for the different analyses: 
#####

#####
##### Subset for 'ASOCIAL ONLY' : 
#####

asocialOnly <- full_data[full_data$copying=="FALSE",]

# create cumulative (asocial) score for each ppt
# using ave and cumsum
asocialOnly$c_a_score <- ave(asocialOnly$score, asocialOnly$ppt, FUN=cumsum)

#probably this whole section (63-76) should instead rank the score of each participant on each question, so that topScorers is an index of their rank, not just a list of the top scorers
# so it could be 
# if asocialOnly$contents !== "Ask Someone Else"
#   for (n in question) 
#      topScorers <- rank(asocialOnly$c_a_score)

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
# don't know what to do about ties, this keeps ties right now, which is fine, but what if ALL are tied for a given question? 
asocialOnly$isMax <- ifelse((asocialOnly$maxScore == asocialOnly$c_a_score),asocialOnly$ppt,NA)
topScorers <- asocialOnly[!is.na(asocialOnly$isMax),]
topScorers <- subset(topScorers, select =c("number","isMax","group","condition"))

#####
##### SUBSET OF COPYING ONLY:
#####

copyOnly <-full_data[full_data$copying=="TRUE",]

# need to link the copied nodes to their ppt ids......
# first make reference dataframe:
node_index <- subset(full_data, select =c(Origin,ppt,group,condition))
node_index <-unique(node_index)

#just select the Ids:
nodeIDS <- (unique(full_data$Origin))
nodeIDS
copyOnlyIds <- copyOnly[copyOnly$Contents%in%nodeIDS,]

#make sure the copied node represents the unique ppts ID not the origin ID.  
for (n in copyOnlyIds$Contents) {
  copyOnlyIds$copied_node <- node_index$ppt[match(copyOnlyIds$Contents, node_index$Origin)]
}


##### Make variable for if they copied highest scorer:
# make a new variable:
copyOnlyIds$topCopy <- rep(NA, length(copyOnlyIds$Contents))

# so if topScorers is now a list of ranks of those who answered themselves, we can say something like the below but need to use 'match' here:
# for (n in numbers)
#  for (g in groups)
#    copyOnlyIds$topCopy <- ifelse (copyOnlyIds$copied_node == min(topScorers$rank)),1,0)
#        



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

#when score info is visible (i.e. round 1 of condition B&C, and when chosen in round 2...)
scoreChoice <- copyOnlyIds[((!copyOnlyIds$condition=="A")&(copyOnlyIds$round==1))|(copyOnlyIds$info_chosen=="Total Score in Round 1"),]                                     

#####
#####
##### Subset for seeing prestige info (Prediction 2)
#####

#get ncopies for each node
ncopies <- tapply(copyOnlyIds$copying, list(copyOnlyIds$copied_node),sum)
ncopies <- as.data.frame(ncopies)
setDT(ncopies, keep.rownames = "copied")
#what if there are more than one maximums, again, does this pick one at random?
maxCopied <- which.max(ncopies$ncopies)

prestigeChoice <- copyOnlyIds[copyOnlyIds$info_chosen =="Times chosen in Round 1",]
prestigeChoice$presCopy <- ifelse((prestigeChoice$copied_node %in% maxCopied),1,0)

#need to account for if the maxCopied wasn't available to copy i.e. was the one copying
#something like:
#prestigeChoice$presCopy <- if(prestigeChoice$ppt! %in% maxCopied) {
#  ifelse((prestigeChoice$Contents=maxCopied),1,0)
#}
# but still what if the top two scorers are both copying... etc etc
# so do we need to do the same for topScorers and create a table of rankings?


#####
##### Subset of info chosen for Prediction 3:
#####
#just the contets of the copying decision (not the nodeIDS) 
copyOnlyContents <- copyOnly[!copyOnly$Contents%in%nodeIDS,]

infoChosen <- copyOnlyContents[copyOnlyContents$round==2,]
infoChosen$chosePrestige <- ifelse(infoChosen$info_chosen=="Times chosen in Round 1",1,0)

# make Condition B the baseline:

infoChosen$CondA <- ifelse(infoChosen$condition =="A", 1, 0)
infoChosen$CondC <- ifelse(infoChosen$condition =="C", 1, 0)

##### 
##### Prediction 4: 
#####
# have copied or not be 0/1 for asocial choices (this includes any 'ask someone else's)

asocialOnly$copied <- ifelse(asocialOnly$Contents=="Ask Someone Else",1,0)

#change baseline to condition to A:
asocialOnly$condB <- ifelse(asocialOnly$condition=="B",1,0)
asocialOnly$condC <- ifelse(asocialOnly$condition=="C",1,0)

#####
##### Prediction 5:
#####

#taking just the accumulated score for the final question for each participant
#for now this is just 8 ppts in condition B (from the full pilot) as the rest was just me playing in different networks and I didn't complete it

finalScore <- asocialOnly[asocialOnly$number==100,]



