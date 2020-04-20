#figuring out how possible "information cascades" were. 
#e.g. how often was there only one player to choose from (and they weren't the top scorer)

full_data <- read.csv("full_data.csv")

asocialOnly <- full_data[full_data$copying=="FALSE",]

copyOnly <-full_data[full_data$copying=="TRUE",]


# first is it a copying decision rather than a copied answer:
model_ids <- full_data[full_data$is_model_id==TRUE,]
# then is it not condition a, and is it in round 1 (of B & C) OR  was info chosen score in R2... 
scoreChoice <- model_ids[((!model_ids$condition=="a")&(model_ids$round==1))|(model_ids$info_chosen=="Total Score in Round 1"),]                                     

#now subset this for just round 1
singleCopying <- model_ids[((!model_ids$condition=="a")&(model_ids$round==1)),]
#and didn't copy top scorer (as this only includes more than one model to copy anyway)
singleCopying <- singleCopying[(singleCopying$copied_successful==0),]

#sort by question number, then contents
singleCopying <- singleCopying[ order(singleCopying$u_network, singleCopying$number), ]
#only certain columns to read easier
singleCopying <- subset(singleCopying, select = c(number, u_network, Contents, u_origin ))
#look for times when more than 4 people were copying on a single question within a single network
table(singleCopying$number, singleCopying$u_network)
