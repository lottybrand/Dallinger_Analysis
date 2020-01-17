
# topic specific scores

# we want this to be asocial, and not give them a score if they chose to copy. If they chose to copy this will be treated as  getting it wrong/ not knowing. 

# load full data:
full_data <- read.csv("full_data.csv")

# these are the question numbers for the topics, in each round:
geography <- c(1:10, 41:55)
weight <- c(11:20, 56:70)
language <-c(21:30, 71:85)
art <- c(31:40, 86:100)

# numbered 1-4 in full data
full_data$topic <- ifelse((full_data$number %in% geography),1,
                            ifelse((full_data$number %in% weight),2,
                              ifelse((full_data$number %in% language),3,
                                ifelse((full_data$number %in% art),4, 999))))

### cumulative ASOCIAL total score (we previously only have this for round 1, or total score including social)
full_data$c_a_score_tot <- rep(-666, nrow(full_data))
u_origins <- unique(full_data$u_origin)
for (i in 1:length(u_origins)) {
  u_origin <- u_origins[i]
  relevant_rows <- c(1:nrow(full_data))[full_data$u_origin == u_origin]
  subset <- full_data[relevant_rows,]
  c_a_score_tot <- cumsum((subset$score) * (1-subset$copying))
  full_data$c_a_score_tot[relevant_rows] <- c_a_score_tot
}

### cumulative asocial score, for GEOGRAPHY
full_data$c_a_score_geog <- rep(-666, nrow(full_data))
u_origins <- unique(full_data$u_origin)
for (i in 1:length(u_origins)) {
  u_origin <- u_origins[i]
  relevant_rows <- c(1:nrow(full_data))[full_data$u_origin == u_origin]
  subset <- full_data[relevant_rows,]
  c_a_score_geog <- cumsum((subset$score) * (1-subset$copying) * (subset$topic==1))
  full_data$c_a_score_geog[relevant_rows] <- c_a_score_geog
}

### cumulative asocial score, for WEIGHT
full_data$c_a_score_wght <- rep(-666, nrow(full_data))
u_origins <- unique(full_data$u_origin)
for (i in 1:length(u_origins)) {
  u_origin <- u_origins[i]
  relevant_rows <- c(1:nrow(full_data))[full_data$u_origin == u_origin]
  subset <- full_data[relevant_rows,]
  c_a_score_wght <- cumsum((subset$score) * (1-subset$copying) * (subset$topic==2))
  full_data$c_a_score_wght[relevant_rows] <- c_a_score_wght
}

### cumulative asocial score, for LANGUAGE 
full_data$c_a_score_lang <- rep(-666, nrow(full_data))
u_origins <- unique(full_data$u_origin)
for (i in 1:length(u_origins)) {
  u_origin <- u_origins[i]
  relevant_rows <- c(1:nrow(full_data))[full_data$u_origin == u_origin]
  subset <- full_data[relevant_rows,]
  c_a_score_lang <- cumsum((subset$score) * (1-subset$copying) * (subset$topic==3))
  full_data$c_a_score_lang[relevant_rows] <- c_a_score_lang
}

### cumulative asocial score, for ART
full_data$c_a_score_art <- rep(-666, nrow(full_data))
u_origins <- unique(full_data$u_origin)
for (i in 1:length(u_origins)) {
  u_origin <- u_origins[i]
  relevant_rows <- c(1:nrow(full_data))[full_data$u_origin == u_origin]
  subset <- full_data[relevant_rows,]
  c_a_score_art <- cumsum((subset$score) * (1-subset$copying) * (subset$topic==4))
  full_data$c_a_score_art[relevant_rows] <- c_a_score_art
}

### make into easy table
asocialOnly <- full_data[full_data$copying=="FALSE",]
finalScore <- asocialOnly[asocialOnly$number==100,]
topicTable <- finalScore[,c(14,23,24,25,26,27)]
colnames(topicTable)[2] <- "Total"
colnames(topicTable)[3] <- "Geography"
colnames(topicTable)[4] <- "Weight"
colnames(topicTable)[5] <- "Language"
colnames(topicTable)[6] <- "Art"

#means 
means <- sapply(topicTable[,2:6],mean)
means
sds <- sapply(topicTable[,2:6],sd)
sds
range <- sapply(topicTable[,2:6],range)
range

#melting tables for plotting
topicTableL <- melt(topicTable, id.vars="u_origin", value.name = "score")
colnames(topicTableL)[2] <- "topic"
topicTableLO <- topicTableL[!topicTableL$topic=="Total",]
# more melting
teenytable <- topicTable
teenytable$u_origin <- NULL
topicTable1 <- melt(teenytable, id.vars="Total", value.name = "score")

#plotting about
boxplot <- ggplot(data = topicTableLO, mapping = aes(x = topic, y = score)) + 
  geom_boxplot()
boxplot

topicsPlot <- ggplot(topicTable1, aes(Total,score)) + 
  geom_point() + 
  stat_smooth() +
  facet_wrap(~variable)
topicsPlot

densityPlot <- ggplot(topicTableLO, aes(score, fill=topic)) 
densityPlot + geom_density(alpha = 0.2) + theme_bw() + 
  theme(text = element_text(size=12), axis.title.y=element_text(margin=margin(0,12,0,0))) +
  scale_y_continuous(limits=c(0,0.12), expand = c(0,0)) +
  scale_x_continuous(limits=c(-5,40), expand= c(0,0)) +
  xlab("topic") + ylab("density")

ggplot(topicTableLO, aes(u_origin,score, col=topic)) + 
  geom_point() + 
  stat_smooth() 

ggplot(data = topicTableLO) + 
  stat_summary(
    mapping = aes(x = topic, y = score),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

# corr tests
cor.test(topicTable$Geography, topicTable$Language)
cor.test(topicTable$Geography, topicTable$Weight)
cor.test(topicTable$Geography, topicTable$Art)
cor.test(topicTable$Art, topicTable$Weight)
cor.test(topicTable$Art, topicTable$Language)
cor.test(topicTable$Weight, topicTable$Language)

