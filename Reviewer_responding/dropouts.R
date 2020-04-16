#Looking out dropout data for reviewer

finished <-finalScore$u_origin
question1 <- full_data[full_data$number==1,]
View(question1)
started <-question1$u_origin
dropOuts <- started %in% finished
summary(dropOuts)

# how do we find when those 58 left? (except manually of course)
# check if dropouts have higher or lower copying behaviour than average?
# check if have higher/lower score than average? 
# anything else?? 
