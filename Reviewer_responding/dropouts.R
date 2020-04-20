#Looking out dropout data for reviewer

finished <-finalScore$u_origin
question1 <- full_data[full_data$number==1,]
View(question1)
started <-question1$u_origin
question1$dropOuts <- started %in% finished

dropOuts <- question1[question1$dropOuts==FALSE,]
dropOuts <- unique(dropOuts$u_origin)
list(dropOuts)

# how do we find the point at which they left (e.g. which question?) except manually of course.

# look at their data up to the point they left and compare to all others? 
# check if dropouts have higher or lower copying behaviour than average?
# check if have higher/lower score than average? 
# check their human tag? 
