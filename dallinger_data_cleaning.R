
# Data prep for Dallinger analysis

source('data_inputting.R')

data_tables <- list(my_data, my_data_a, my_data_c)

my_data$uid <- my_data$id
my_data$u_origin <- my_data$Origin
my_data$u_network <- my_data$Network

full_data <- my_data

for (i in 2:length(data_tables)) {
  this_table <- data_tables[[i]]
  this_table$uid <- this_table$id + max(full_data$uid)
  this_table$u_origin <- this_table$Origin + max(full_data$u_origin)
  this_table$u_network <- this_table$Network + max(full_data$u_network)
  full_data <- bind_rows(full_data, this_table)
}

##### Merge data from all conditions after data_unputting.R
#full_data <- bind_rows(my_data,my_data_a,my_data_c)

#because the info 'Origin' is repeated each time we run it, we need to reindex participants based on their condition and network
#start by ordering according to condition, network and number(question)
full_data$number <- as.integer(full_data$number)
# full_data <- full_data[order(condition,Network,number),]

# This is getting close but it's not treating new conditions as new sets...
#full_data$num <- with(full_data, ave(Origin, condition, FUN=function(x) match(x, unique(Origin))))

#this just starts at 1 and keeps iterating up. some combo of this and above..?
#full_data$num <- ave(full_data$Origin, full_data$condition, FUN = seq_along)

# don't know what this thinks it's doing: 
#for (i in unique(full_data$Origin)) full_data$num[full_data$Origin == i] <- seq_len(sum(full_data$Origin == i))

#my own hacky attempt for now:
# full_data$ppt <- with(full_data, ave(Origin, condition, FUN=function(x) match(x, unique(x))))
# pptsA <- length(unique(my_data_a$Origin))
# 
# full_data$ppt <- ifelse(full_data$condition=="B", full_data$ppt+pptsA,full_data$ppt)
# 
# pptsB <- length(unique(my_data$Origin))
# full_data$ppt <- ifelse(full_data$condition=="C", full_data$ppt+pptsA+pptsB,full_data$ppt)
# 
# # manually reindex network too for now, as in full run they will be individual (at least per condition)
# # will need to automate this for the real thing, maybe like the above?
# full_data$group <- ifelse((full_data$condition=="B"),3,
#                           ifelse((full_data$condition=="C" & full_data$Network==1),4,
#                             ifelse((full_data$condition=="C" & full_data$Network==2), 5, full_data$Network)))

#####
##### Subsets for the different analyses: 
#####

#####
##### Subset for 'ASOCIAL ONLY' : 
#####

full_data$c_a_score <- rep(-666, nrow(full_data))
for (i in 1:nrow(full_data)) {
  c_a_score <- sum(full_data[1:i,]$score[full_data$copying == FALSE & full_data$u_origin == full_data$u_origin[i]])
  if (is.na(c_a_score)) {
    c_a_score <- 0
  }
  full_data$c_a_score[i] <- c_a_score
}

full_data$c_copies <- rep(-666, nrow(full_data))
all_node_ids <- unique(full_data$Origin)
full_data$is_model_id <- (full_data$copying == TRUE & full_data$Contents %in% all_node_ids)
for (i in 1:nrow(full_data)) {
  full_data$c_copies[i] <- nrow(full_data[
    full_data$uid < full_data$uid[i] & 
    full_data$is_model_id == TRUE &
    full_data$u_network == full_data$u_network[i] &
    full_data$Contents == as.character(full_data$Origin[i])
  ,])
}

full_data$copied_winner <- rep("-666", nrow(full_data))
for (i in 1:nrow(full_data)) {
  if (full_data$is_model_id[i] == FALSE) {
    full_data$copied_winner[i] <- NA
  } else {
    models <- full_data[full_data$number == full_data$number[i] & full_data$u_network == full_data$u_network[i] & full_data$copying == FALSE,]
    model <- models[as.character(models$Origin) == full_data$Contents[i],]
    if (nrow(models) <= 1) {
      full_data$copied_winner[i] <- NA
    } else {
      if (length(unique(models$c_a_score)) == 1) {
        full_data$copied_winner[i] <- NA
      } else {
        full_data$copied_winner[i] <- (model$c_a_score == max(models$c_a_score))
      }
    }
  }
}

#asocialOnly <- full_data[full_data$copying=="FALSE",]

# create cumulative (asocial) score for each ppt
# using ave and cumsum
#asocialOnly$c_a_score <- ave(asocialOnly$score, asocialOnly$ppt, FUN=cumsum)

#create database of those who actually answered asocially for each question
#answeredRanks <- asocialOnly[asocialOnly$Contents!="Ask Someone Else",]

#rank them according to their accumulated score on that question, per question, per group:
#numbers <- unique(answeredRanks$number)
#groups <- unique(answeredRanks$group)

#for (n in numbers) 
#{
#  for (g in groups) 
#  {
#    answeredRanks$rank[answeredRanks$number == n & answeredRanks$group ==g] <- rank(answeredRanks$c_a_score[answeredRanks$number == n & answeredRanks$group ==g],)
#  }
#}

#clean it up
#answeredRanks <- subset(answeredRanks, select = c("number","group","c_a_score","ppt","rank"))
#this seems to do it, but note 1 is lowest rank and higher rank = higher score. 
#still need to address when everyone is on the same score (ie when all ppts' ranks = 1 for a given group for a given question)


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
# this does not appear to work right now: 

groups <- unique(node_index$group)

for (g in groups) {
  copyOnlyIds$copied_node[node_index$group==g] <- node_index$ppt[match(copyOnlyIds$Contents[node_index$group==g], node_index$Origin[node_index$group==g])]
}


##### Make variable for if they copied highest scorer:
# make a new variable:
copyOnlyIds$topCopy <- rep(NA, length(copyOnlyIds$Contents))

# so we now have a list of ranks of those who answered themselves
# also what does it do if there is no max? e.g. all the same, or two maximums?


#this should be working, but need to fix above first:
numbers <- unique(answeredRanks$number)
groups <- unique(answeredRanks$group)

for (n in numbers) 
  {
  for (g in groups) 
    {
  copyOnlyIds$topCopy[answeredRanks$number == n & answeredRanks$group ==g] <- ifelse((copyOnlyIds$copied_node[answeredRanks$number == n & answeredRanks$group == g]) == max(answeredRanks$rank[answeredRanks$number == n & answeredRanks$group ==g]),1,0)
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



