
# Data prep for Dallinger analysis
# SKIP TO LINE 113 <<- ONLY NEED TO RUN THIS
# source('data_inputting.R')

#####
#assign first dataset to full_data:
full_data <- loaded_files[[1]]

#assign first dataset unique info ids and network ids
full_data$uid <- full_data$id
full_data$u_network <- full_data$Network
full_data$u_origin <- full_data$Origin

#give unique ids to each subsequent datasets by adding the max of the previous id's 
for (i in 2:length(loaded_files)) {
  this_table <- loaded_files[[i]]
  this_table$uid <- this_table$id + max(full_data$uid)
  this_table$u_origin <- this_table$Origin + max(full_data$u_origin)
  this_table$u_network <- this_table$Network + max(full_data$u_network)
  full_data <- bind_rows(full_data, this_table)
}

current_u_origins <- unique(full_data$u_origin)
full_data$u_origin <- match(full_data$u_origin, current_u_origins)


#####
##### Calculating the cumulative score, cumulative copies, and if they copied the top scorer and most copied : 
#####

#figure out cumulative ASOCIAL score in Round 1
full_data$c_a_score_r1 <- rep(-666, nrow(full_data))
u_origins <- unique(full_data$u_origin)
for (i in 1:length(u_origins)) {
  u_origin <- u_origins[i]
  relevant_rows <- c(1:nrow(full_data))[full_data$u_origin == u_origin]
  subset <- full_data[relevant_rows,]
  c_a_score_r1 <- cumsum(subset$score * (1-subset$copying) * (subset$round==1))
  full_data$c_a_score_r1[relevant_rows] <- c_a_score_r1
}

#figure out total score including copies 
full_data$t_score <- rep(-666, nrow(full_data))
u_origins <- unique(full_data$u_origin)
for (i in 1:length(u_origins)) {  
  u_origin <- u_origins[i]
  relevant_rows <- c(1:nrow(full_data))[full_data$u_origin == u_origin]
  subset <- full_data[relevant_rows,]
  t_score <- cumsum(subset$score)
  full_data$t_score[relevant_rows] <- t_score
}

#figure out total copied
full_data$t_copied <- rep(-666, nrow(full_data))
u_origins <- unique(full_data$u_origin)
for (i in 1:length(u_origins)) {  
  u_origin <- u_origins[i]
  relevant_rows <- c(1:nrow(full_data))[full_data$u_origin == u_origin & full_data$is_model_id == FALSE]
  subset <- full_data[relevant_rows,]
  t_copied <- cumsum(subset$copying)
  full_data$t_copied[relevant_rows] <- t_copied
}

#figure out their cumulative copies:

full_data$c_copies <- rep(-666, nrow(full_data))
all_node_ids <- unique(full_data$Origin)
full_data$is_model_id <- (full_data$copying == TRUE & full_data$Contents %in% all_node_ids)
copying_decisions <- full_data[full_data$is_model_id == TRUE,]
for (i in 1:nrow(full_data)) {
  full_data$c_copies[i] <- nrow(copying_decisions[
    copying_decisions$round == 1 &
    copying_decisions$u_network == full_data$u_network[i] &
    copying_decisions$uid < full_data$uid[i] & 
    copying_decisions$Contents == as.character(full_data$Origin[i])
  ,])
}

#figure out if they copied the highest scorer: 
full_data$copied_successful <- rep(NA, nrow(full_data))
potential_models <- full_data[full_data$copying == FALSE & full_data$Contents != "Ask Someone Else",]
for (i in 1:nrow(full_data)) {
  if (full_data$is_model_id[i] == TRUE) {
    models <- potential_models[potential_models$number == full_data$number[i] & potential_models$u_network == full_data$u_network[i],]
    if (nrow(models) > 1) {
      if (length(unique(models$c_a_score_r1)) != 1) {
        model <- models[as.character(models$Origin) == full_data$Contents[i],]
        full_data$copied_successful[i] <- (model$c_a_score_r1 == max(models$c_a_score_r1))*1
      }
    }
  }
}

#figure out if they copied the most copied:
full_data$copied_prestigious <- rep(NA, nrow(full_data))
potential_models <- full_data[full_data$copying == FALSE & full_data$Contents != "Ask Someone Else",]
for (i in 1:nrow(full_data)) {
  if (full_data$is_model_id[i] == TRUE) {
    models <- potential_models[potential_models$number == full_data$number[i] & potential_models$u_network == full_data$u_network[i],]
    if (nrow(models) > 1) {
      if (length(unique(models$c_copies)) != 1) {
        model <- models[as.character(models$Origin) == full_data$Contents[i],]
        full_data$copied_prestigious[i] <- (model$c_copies == max(models$c_copies))*1
      }
    }
  }
}

#####
##### SKIP TO HERE IN FUTURE
#write.csv(full_data, file="full_data.csv", row.names = FALSE)
#full_data <- read.csv("full_data.csv")
# saveRDS(full_data, "full_data")

#####
##### SUBSET OF ASOCIAL ONLY: 
#####
asocialOnly <- full_data[full_data$copying=="FALSE",]

#####
##### SUBSET OF COPYING ONLY:
#####

copyOnly <-full_data[full_data$copying=="TRUE",]

#####
##### Subset for seeing score info (Prediction 1)
#####

# need to subset only copying instances and only when score info is visible (i.e. round 1 of condition B&C, and when chosen in round 2...)
# first is it a copying decision rather than a copied answer:
model_ids <- full_data[full_data$is_model_id==TRUE,]
# then is it not condition a, and is it in round 1 (of B & C) OR  was info chosen score in R2... 
scoreChoice <- model_ids[((!model_ids$condition=="a")&(model_ids$round==1))|(model_ids$info_chosen=="Total Score in Round 1"),]                                     


#####
##### Subset for seeing prestige info (Prediction 2)
#####

prestigeChoice <- model_ids[model_ids$info_chosen =="Times chosen in Round 1",]


#####
##### Subset of info chosen for Predictions 3,4,5 (models 3 and 3.1):
#####

#just the contets of the copying decision (not the nodeIDS) 
copyOnlyContents <- copyOnly[!copyOnly$is_model_id==TRUE,]

infoChosen <- copyOnlyContents[copyOnlyContents$round==2,]
infoChosen$chosePrestige <- ifelse(infoChosen$info_chosen=="Times chosen in Round 1",1,0)

# make Condition B the baseline for old approach:
#infoChosen$CondA <- ifelse(infoChosen$condition =="a", 1, 0)
#infoChosen$CondC <- ifelse(infoChosen$condition =="c", 1, 0)

##### 
##### Prediction 6 (model4): 
#####

# have copied or not be 0/1 for asocial choices (this includes any 'ask someone else's)

asocialOnly$copied <- ifelse(asocialOnly$Contents=="Ask Someone Else",1,0)
asocialOnly_2 <- asocialOnly[asocialOnly$round==2,]

#change baseline to condition to A if not using ulam method:
#asocialOnly$condB <- ifelse(asocialOnly$condition=="b",1,0)
#asocialOnly$condC <- ifelse(asocialOnly$condition=="c",1,0)

#####
##### Prediction 7 (model 5):
#####

#taking just the accumulated score for the final question for each participant
finalScore <- asocialOnly[asocialOnly$number==100,]
finalScoreBC <- finalScore[!finalScore$condition=="a",]
finalScoreA <- finalScore[finalScore$condition=="a",]

#what about just round 2's total scores:
full_data_R2 <- full_data[full_data$round==2,]
full_data_R2$t_score_r2 <- rep(-666, nrow(full_data_R2))
u_origins <- unique(full_data_R2$u_origin)
for (i in 1:length(u_origins)) {  
  u_origin <- u_origins[i]
  relevant_rows <- c(1:nrow(full_data_R2))[full_data_R2$u_origin == u_origin]
  subset <- full_data_R2[relevant_rows,]
  t_score_r2 <- cumsum(subset$score)
  full_data_R2$t_score_r2[relevant_rows] <- t_score_r2
}

asocialOnly_2 <- full_data_R2[full_data_R2$copying=="FALSE",]

finalScore_R2 <- asocialOnly_2[asocialOnly_2$number==100,]
