#plotting 
# run relevant sections of analysis_script first
hist(finalScore$c_copies)
hist(finalScore$t_copied)


prestigePlot <- ggplot(data = finalScore) + 
  stat_count(mapping = aes(x = c_copies), )
prestigePlot + theme_bw() + 
  theme(text = element_text(size=12), axis.title.y=element_text(margin=margin(0,12,0,0))) +
  scale_y_continuous(limits=c(0,30), expand = c(0,0)) +
  scale_x_continuous(limits=c(0,70), expand= c(0,0)) +
  xlab("Number of times copied") + ylab("Number of Participants")

plot(finalScore$c_copies ~ finalScore$u_origin)

finalScore$groupIndex <- as.factor(finalScore$groupIndex)

presPlot2<- ggplot(data = finalScore, mapping = aes(x = u_origin, y = c_copies, color = groupIndex)) + 
  geom_point(size=3)
presPlot2 + theme_bw() + 
  theme(text = element_text(size=12), axis.title.y=element_text(margin=margin(0,12,0,0))) +
  xlab("Participant ID") + ylab("Prestige Score (Total times Copied)")

plot(finalScore$c_copies ~ finalScore$t_score)

scorePresPlot <- ggplot(data = finalScore, mapping = aes(x = t_score, y = c_copies)) + 
  geom_point() + 
  geom_smooth() + theme_bw() + xlab("Total Score") + ylab("Prestige Score")
scorePresPlot

finalScoreBC <- finalScore[!finalScore$condition=="a",]

scorePresPlotBC <- ggplot(data = finalScoreBC, mapping = aes(x = t_score, y = c_copies)) + 
  geom_point(size=3) 
scorePresPlotBC + theme_bw() +
  theme(text = element_text(size=12), axis.title.y=element_text(margin=margin(0,12,0,0))) +
  xlab("Total Score") + ylab("Prestige Score (Total times Copied)")


cor.test(finalScoreBC$c_copies, finalScoreBC$t_score)