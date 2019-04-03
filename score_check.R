#subset to check score info from dallinger output: 

scoreCheck <- subset(asocialOnly, select = c(number, score, Origin, c_a_score))

asoc_total_score <- tapply(scoreCheck$score, list(scoreCheck$Origin),sum)
asoc_total_score

copy_score <- tapply(copyOnly$score, list(copyOnly$Origin),sum)
copy_score

total_score <- tapply(clean_df$score, list(clean_df$Origin),sum)
total_score

nodeIDs <- c(2,3,4,5,6,7,8,10,11)
copyOnlyContents <- copyOnly[!copyOnly$Contents%in%nodeIDs,]
copyOnlyIds <- copyOnly[copyOnly$Contents%in%nodeIDs,]

ncopies <- tapply(copyOnlyIds$copying, list(copyOnlyIds$Contents),sum)
ncopies

asoc_R1 <- asocialOnly[asocialOnly$round ==1,]
asoc_R1_score <- tapply(asoc_R1$score, list(asoc_R1$Origin),sum)                   
asoc_R1_score
