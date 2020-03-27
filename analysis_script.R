
#### Analyses for preregistration #####
#### Run dallinger_data_cleaning.R script first #####

#install.packages(c("coda","mvtnorm","devtools","loo","dagitty"))
#library(devtools)
#devtools::install_github("rmcelreath/rethinking",ref="Experimental")
#library(rethinking)

#source('dallinger_data_cleaning.R') 

#####
#####
##### Prediction 1: Participants copy the highest scoring participant out of those available in Conditions B & C
#####
#####
#####

# data frame is whenever a copying event happened && score information was available 
# =(Round 1 in Condition B & C, Round 2 in Condition C depending on Choice.)
# dataframe is made in dallinger_data_cleaning.R file

scoreChoice<- as.data.frame(scoreChoice)

#make index contiguous for participant varying effect:
Nppts = length(unique(scoreChoice$u_origin))
Oldppt <- scoreChoice$u_origin
pptIndex <- array(0,length(scoreChoice$u_origin))
for (index in 1:Nppts){
  pptIndex[Oldppt == unique(Oldppt)[index]] = index
}
scoreChoice$pptIndex <- pptIndex

#make index contiguous for group varying effect:
Ngroups = length(unique(scoreChoice$u_network))
Oldgroup <- scoreChoice$u_network
groupIndex <- array(0,length(scoreChoice$u_network))
for (index in 1:Ngroups){
  groupIndex[Oldgroup == unique(Oldgroup)[index]] = index
}
scoreChoice$groupIndex <- groupIndex


model1 <- map2stan(
  alist(
    copied_successful ~ dbinom(1, p),
    logit(p) <- a + a_p[pptIndex]*sigma_p + a_g[groupIndex]*sigma_g,
    a ~ dnorm(0,2),
    a_p[pptIndex] ~ dnorm(0,1),
    a_g[groupIndex] ~ dnorm(0,1),
    sigma_p ~ dcauchy(0,1),
    sigma_g ~ dcauchy(0,1)
  ),
  data=scoreChoice, constraints=list(sigma_p="lower=0", sigma_g="lower=0"), 
  warmup=1000, iter=4000, chains=3, cores=3)

precis(model1)

#####
#####
#####
##### Prediction 2: Participants copy the most-copied participant out of those available in Round 2 of Conditions B & C 
#####
#####
#####

## Dataframe is whenever someone chose to view prestige information when based on success (Conds B&C Only), made in dallinger_data_cleaning.R 
## A separate, identical model will be run for any prestige-based copying that occurs in Condition A, as prestige cues in Condition A are not based on success information (see pre-reg file), so we have no a priori predictions for this behaviour

prestigeChoice<- as.data.frame(prestigeChoice)
prestigeChoice <- prestigeChoice[!prestigeChoice$condition=="a",]

#make ppt index contiguous:
Nppts = length(unique(prestigeChoice$u_origin))
Oldppt <- prestigeChoice$u_origin
pptIndex <- array(0,length(prestigeChoice$u_origin))
for (index in 1:Nppts){
  pptIndex[Oldppt == unique(Oldppt)[index]] = index
}
prestigeChoice$pptIndex <- pptIndex

#make group index contiguous:
Ngroups = length(unique(prestigeChoice$u_network))
Oldgroup <- prestigeChoice$u_network
groupIndex <- array(0,length(prestigeChoice$u_network))
for (index in 1:Ngroups){
  groupIndex[Oldgroup == unique(Oldgroup)[index]] = index
}
prestigeChoice$groupIndex <- groupIndex

model2 <- map2stan(
  alist(
    copied_prestigious ~ dbinom(1, p),
    logit(p) <- a + a_p[pptIndex]*sigma_p + a_g[groupIndex]*sigma_g,
    a ~ dnorm(0,2),
    a_p[pptIndex] ~ dnorm(0,1),
    a_g[groupIndex] ~ dnorm(0,1),
    sigma_p ~ dcauchy(0,1),
    sigma_g ~ dcauchy(0,1)
  ),
  data=prestigeChoice, constraints=list(sigma_p="lower=0", sigma_g="lower=0"), 
  warmup=1000, iter=4000, chains=3, cores=3 )

precis(model2)

#now just for condition A prestige copying:
prestigeChoiceA <- model_ids[model_ids$info_chosen =="Times chosen in Round 1" & model_ids$condition=="a",]
prestigeChoiceA<- as.data.frame(prestigeChoiceA)

#need to reindex again
Nppts = length(unique(prestigeChoiceA$u_origin))
Oldppt <- prestigeChoiceA$u_origin
pptIndex <- array(0,length(prestigeChoiceA$u_origin))
for (index in 1:Nppts){
  pptIndex[Oldppt == unique(Oldppt)[index]] = index
}
prestigeChoiceA$pptIndex <- pptIndex

#make group index contiguous:
Ngroups = length(unique(prestigeChoiceA$u_network))
Oldgroup <- prestigeChoiceA$u_network
groupIndex <- array(0,length(prestigeChoiceA$u_network))
for (index in 1:Ngroups){
  groupIndex[Oldgroup == unique(Oldgroup)[index]] = index
}
prestigeChoiceA$groupIndex <- groupIndex


#for Condition A prestige-copying only, if any occurs:
model2.1 <- map2stan(
  alist(
    copied_prestigious ~ dbinom(1, p),
    logit(p) <- a + a_p[pptIndex]*sigma_p + a_g[groupIndex]*sigma_g,
    a ~ dnorm(0,2),
    a_p[pptIndex] ~ dnorm(0,1),
    a_g[groupIndex] ~ dnorm(0,1),
    sigma_p ~ dcauchy(0,1),
    sigma_g ~ dcauchy(0,1)
  ),
  data=prestigeChoiceA, constraints=list(sigma_p="lower=0", sigma_g="lower=0"), 
  warmup=1000, iter=1000, chains=1, cores=1 )

precis(model2.1)
#####
#####
##### Prediction 3,4 and 5: Participants choose to view “most copied” info more in Condition B than the other two conditions, 
##### because (i) in Condition B copiers can access success info, unlike Condition A where copiers only have access to irrelevant info,
##### and (ii) in Condition B copying info is the only relevant cue available, unlike Condition C where direct success info is available and just as easily accessible
#####
#####
#####

# Dataframe is whenever a copying decision was made in round 2 in all conditions 
# made in dallinger_data_cleaning.R

#make index contiguous for varying effects:

infoChosen <- as.data.frame(infoChosen)

Nppts = length(unique(infoChosen$u_origin))
Oldppt <- infoChosen$u_origin
pptIndex <- array(0,length(infoChosen$u_origin))
for (index in 1:Nppts){
  pptIndex[Oldppt == unique(Oldppt)[index]] = index
}
infoChosen$pptIndex <- pptIndex

Ngroups = length(unique(infoChosen$u_network))
Oldgroup <- infoChosen$u_network
groupIndex <- array(0,length(infoChosen$u_network))
for (index in 1:Ngroups){
  groupIndex[Oldgroup == unique(Oldgroup)[index]] = index
}
infoChosen$groupIndex <- groupIndex

### the ulam version using Statistical Rethinking 2nd Edition
### make condition an index rather than using dummy variables (pp 328 Statistical Rethinking 2nd Edition )

Nconds = length(unique(infoChosen$condition))
Oldconds <- infoChosen$condition
condsIndex <- array(0,length(infoChosen$condition))
for (index in 1:Nconds){
  condsIndex[Oldconds == unique(Oldconds)[index]] = index
}
infoChosen$condsIndex <- condsIndex

infoChosen$chosePrestige <-as.integer(infoChosen$chosePrestige)
infoChosen$condsIndex <- as.integer(infoChosen$condsIndex)
infoChosen$pptIndex <- as.integer(infoChosen$pptIndex)
infoChosen$groupIndex <- as.integer(infoChosen$groupIndex)

infoChosen_list <- list(
  chosePrestige = infoChosen$chosePrestige,
  pptIndex = infoChosen$pptIndex,
  groupIndex = infoChosen$groupIndex,
  condsIndex = infoChosen$condsIndex )

model3 <- ulam(
  alist(
    chosePrestige ~ dbinom( 1 , p ) ,
    logit(p) <- a[pptIndex] + g[groupIndex] + b[condsIndex] ,
    b[condsIndex] ~ dnorm( 0 , 0.5 ),
    a[pptIndex] ~ dnorm( a_bar , sigma_a ),
    g[groupIndex] ~ dnorm( 0 , sigma_g ),
    a_bar ~ dnorm( 0 , 1.5 ),
    sigma_a ~ dexp(1),
    sigma_g ~ dexp(1),
    sigma_b ~ dexp(1)
  ) , data=infoChosen_list, constraints=list(sigma_a="lower=0", sigma_g="lower=0", sigma_b="lower=0"), 
  warmup=1000, iter=4000, chains=3 , cores=3 , log_lik=TRUE )

precis(model3)

#plotting predictions based on conds: (pp 332 in 2nd edition )
post <- extract.samples(model3)
p_conds <- inv_logit( post$b )
plot( precis( as.data.frame(p_conds) ) , xlim=c(0,1) )

#plotting condition effects, (pp 333 in 2nd edition)
plot( precis( model3 , depth=2 , pars="b" ))

#now implementing condition as varying intercepts too, pp. 423 in Statistical Rethinking 2nd Edition:
#this version was used for inference (see results file)
model3.1 <- ulam(
  alist(
    chosePrestige ~ dbinom( 1 , p ) ,
    logit(p) <- a[pptIndex] + g[groupIndex] + b[condsIndex] ,
    b[condsIndex] ~ dnorm( 0 , sigma_b ),
    a[pptIndex] ~ dnorm( a_bar , sigma_a ),
    g[groupIndex] ~ dnorm( 0 , sigma_g ),
    a_bar ~ dnorm( 0 , 1.5 ),
    sigma_a ~ dexp(1),
    sigma_g ~ dexp(1),
    sigma_b ~ dexp(1)
  ) , data=infoChosen_list, constraints=list(sigma_a="lower=0", sigma_g="lower=0", sigma_b="lower=0"), control=list( adapt_delta=0.99, max_treedepth=13), 
  warmup=1000, iter=9000, chains=3 , cores=3 , log_lik=TRUE )

precis(model3.1, depth = 2)
precis(model3.1, pars = c('b[1]', 'b[2]', 'b[3]'), depth=2)
traceplot(model3.1)

#plotting condition effects, (pp 333 in 2nd edition)
mainFig <- plot(precis(model3.1, depth = 2), pars=c("b[2]","b[1]","b[3]"), labels=c("Control \nCondition","Prestige \nCondition","Success \nCondition"), xlab="Model estimate")
title("Participants Chose Prestige")

#####
#####
##### Prediction 6 (model4): Copying rate is higher in Conditions B & C compared to Condition A because copying is only based on success in Conditions B & C
#####
#####

## Data frame is all individual answers to each question (asocialOnly) 
## which includes a column for if they chose "ask someone else" made in dallinger_data_cleaning.R

#let's try ulam from 2nd edition again: 

asocialOnly_2 <- as.data.frame(asocialOnly_2)

Nppts = length(unique(asocialOnly_2$u_origin))
Oldppt <- asocialOnly_2$u_origin
pptIndex <- array(0,length(asocialOnly_2$u_origin))
for (index in 1:Nppts){
  pptIndex[Oldppt == unique(Oldppt)[index]] = index
}
asocialOnly_2$pptIndex <- pptIndex

Ngroups = length(unique(asocialOnly_2$u_network))
Oldgroup <- asocialOnly_2$u_network
groupIndex <- array(0,length(asocialOnly_2$u_network))
for (index in 1:Ngroups){
  groupIndex[Oldgroup == unique(Oldgroup)[index]] = index
}
asocialOnly_2$groupIndex <- groupIndex

Nconds = length(unique(asocialOnly_2$condition))
Oldconds <- asocialOnly_2$condition
condsIndex <- array(0,length(asocialOnly_2$condition))
for (index in 1:Nconds){
  condsIndex[Oldconds == unique(Oldconds)[index]] = index
}
asocialOnly_2$condsIndex <- condsIndex


asocialOnly_2$copied <-as.integer(asocialOnly_2$copied)
asocialOnly_2$condsIndex <- as.integer(asocialOnly_2$condsIndex)
asocialOnly_2$pptIndex <- as.integer(asocialOnly_2$pptIndex)
asocialOnly_2$groupIndex <- as.integer(asocialOnly_2$groupIndex)

asocialOnly_list_2 <- list(
  copied = asocialOnly_2$copied,
  pptIndex = asocialOnly_2$pptIndex,
  groupIndex = asocialOnly_2$groupIndex,
  condsIndex = asocialOnly_2$condsIndex )

model4 <- ulam(
  alist(
    copied ~ dbinom( 1 , p ) ,
    logit(p) <- a[pptIndex] + g[groupIndex] + b[condsIndex] ,
    b[condsIndex] ~ dnorm( 0 , sigma_b ),
    a[pptIndex] ~ dnorm( a_bar , sigma_a ),
    g[groupIndex] ~ dnorm( 0 , sigma_g ),
    a_bar ~ dnorm( 0 , 1.5 ),
    sigma_a ~ dexp(1),
    sigma_g ~ dexp(1),
    sigma_b ~ dexp(1)
  ) , data=asocialOnly_list_2 , constraints=list(sigma_a="lower=0", sigma_g="lower=0", sigma_b="lower=0"), control=list( adapt_delta=0.99, max_treedepth=13), 
  warmup=1000, iter=5000, chains=3 , cores=3 , log_lik=TRUE )

precis(model4)
precis(model4, depth = 2)
precis(model4, pars = c('a_bar','b[1]', 'b[2]', 'b[3]'), depth=2)
traceplot(model4)

table(asocialOnly_2$copied)
tapply(asocialOnly_2$copied, list(asocialOnly_2$condition),sum)

table(asocialOnly_2$condition)

#model4 trouble with convergence, reparameterised (from the book!)
model4.2 <- ulam(
  alist(
    copied ~ dbinom( 1 , p ) ,
    logit(p) <- a_bar + a[pptIndex]*sigma_a + g[groupIndex]*sigma_g + b[condsIndex] ,
    b[condsIndex] ~ dnorm( 0 , 0.5 ),
    a[pptIndex] ~ dnorm( 0 , 1 ),
    g[groupIndex] ~ dnorm( 0 , 1 ),
    a_bar ~ dnorm( 0 , 1.5 ),
    sigma_a ~ dexp(1),
    sigma_g ~ dexp(1)
  ) , data=asocialOnly_list_2 , constraints=list(sigma_a="lower=0", sigma_g="lower=0"), control=list( adapt_delta=0.99, max_treedepth=13), 
  warmup=1000, iter=5000, chains=3 , cores=3 , log_lik=TRUE )
print(Sys.time())

precis(model4.2)
precis(model4.2, depth = 2)
precis(model4.2, pars = c('a_bar','b[1]', 'b[2]', 'b[3]'), depth=2)
traceplot(model4)

post <- extract.samples(model4.2)
diff_ab <- post$b[,1] - post$b[,2]
diff_ac <- post$b[,3] - post$b[,2]
diff_bc <- post$b[,1] - post$b[,3]
precis(list(diff_ab=diff_ab, diff_ac=diff_ac, diff_bc=diff_bc))


### without a_bar (discussions with Tom on Slack over a_bar)
model4.3 <- ulam(
  alist(
    copied ~ dbinom( 1 , p ) ,
    logit(p) <- a[pptIndex]*sigma_a + g[groupIndex]*sigma_g + b[condsIndex] ,
    b[condsIndex] ~ dnorm( 0 , 1 ),
    a[pptIndex] ~ dnorm( 0 , 1 ),
    g[groupIndex] ~ dnorm( 0 , 1 ),
    sigma_a ~ dexp(1),
    sigma_g ~ dexp(1)
  ) , data=asocialOnly_list_2 , constraints=list(sigma_a="lower=0", sigma_g="lower=0"), control=list( adapt_delta=0.99, max_treedepth=13), 
  warmup=1000, iter=5000, chains=3 , cores=3 , log_lik=TRUE )
print(Sys.time())

precis(model4.3, depth = 2)
precis(model4.3, pars = c('b[1]', 'b[2]', 'b[3]'), depth=2)


post4.3 <- extract.samples(model4.3)
diff_ab_4.3 <- post4.3$b[,1] - post4.3$b[,2]
diff_ac_4.3 <- post4.3$b[,3] - post4.3$b[,2]
diff_bc_4.3 <- post4.3$b[,1] - post4.3$b[,3]
precis(list(diff_ab_4.3=diff_ab_4.3, diff_ac_4.3=diff_ac_4.3, diff_bc_4.3=diff_bc_4.3))


#put a_bar back in, check it's not the priors...
model4.4 <- ulam(
  alist(
    copied ~ dbinom( 1 , p ) ,
    logit(p) <- a_bar + a[pptIndex]*sigma_a + g[groupIndex]*sigma_g + b[condsIndex],
    b[condsIndex] ~ dnorm( 0 , 1 ),
    a[pptIndex] ~ dnorm( 0 , 1 ),
    g[groupIndex] ~ dnorm( 0 , 1 ),
    a_bar ~ dnorm ( 0, 1 ),
    sigma_a ~ dexp(1),
    sigma_g ~ dexp(1)
  ) , data=asocialOnly_list_2 , constraints=list(sigma_a="lower=0", sigma_g="lower=0"), control=list( adapt_delta=0.99, max_treedepth=13), 
  warmup=1000, iter=5000, chains=3 , cores=3 , log_lik=TRUE )
print(Sys.time())

precis(model4.4, depth = 2)
precis(model4.4, pars = c('a_bar','b[1]', 'b[2]', 'b[3]'), depth=2)

post4.4 <- extract.samples(model4.4)
diff_ab_4.4 <- post4.4$b[,1] - post4.4$b[,2]
diff_ac_4.4 <- post4.4$b[,3] - post4.4$b[,2]
diff_bc_4.4 <- post4.4$b[,1] - post4.4$b[,3]
precis(list(diff_ab_4.4=diff_ab_4.4, diff_ac_4.4=diff_ac_4.4, diff_bc_4.4=diff_bc_4.4))

#Altering priors with a_bar (after discussions with Tom on Slack and McElreath on Twitter...)
model4.5 <- ulam(
  alist(
    copied ~ dbinom( 1 , p ) ,
    logit(p) <- a_bar + a[pptIndex]*sigma_a + g[groupIndex]*sigma_g + b[condsIndex],
    b[condsIndex] ~ dnorm( 0 , 0.5 ),
    a[pptIndex] ~ dnorm( 0 , 0.5 ),
    g[groupIndex] ~ dnorm( 0 , 0.5 ),
    a_bar ~ dnorm ( 0, 1.5 ),
    sigma_a ~ dexp(1),
    sigma_g ~ dexp(1)
  ) , data=asocialOnly_list_2 , constraints=list(sigma_a="lower=0", sigma_g="lower=0"), control=list( adapt_delta=0.99, max_treedepth=13), 
  warmup=1000, iter=5000, chains=3 , cores=3 , log_lik=TRUE )
print(Sys.time())

precis(model4.5, depth = 2)
precis(model4.5, pars = c('a_bar','b[1]', 'b[2]', 'b[3]'), depth=2)

post4.5 <- extract.samples(model4.5)
diff_ab_4.5 <- post4.5$b[,1] - post4.5$b[,2]
diff_ac_4.5 <- post4.5$b[,3] - post4.5$b[,2]
diff_bc_4.5 <- post4.5$b[,1] - post4.5$b[,3]
precis(list(diff_ab_4.5=diff_ab_4.5, diff_ac_4.5=diff_ac_4.5, diff_bc_4.5=diff_bc_4.5))



#####
#####
##### Prediction 7 (model5): Participants perform best on the quiz in Condition B & C compared to Condition A because copying is only based on success in Conditions B & C
#####

## Data frame consists of accumulated scores (including copied score) on final question
## made in dallinger_data_cleaning.R 

## try Ulam again from 2nd Edition: 

finalScore <- as.data.frame(finalScore)


Ngroups = length(unique(finalScore$u_network))
Oldgroup <- finalScore$u_network
groupIndex <- array(0,length(finalScore$u_network))
for (index in 1:Ngroups){
  groupIndex[Oldgroup == unique(Oldgroup)[index]] = index
}
finalScore$groupIndex <- groupIndex
finalScore$groupIndex <- as.integer(finalScore$groupIndex)

Nconds = length(unique(finalScore$condition))
Oldconds <- finalScore$condition
condsIndex <- array(0,length(finalScore$condition))
for (index in 1:Nconds){
  condsIndex[Oldconds == unique(Oldconds)[index]] = index
}
finalScore$condsIndex <- condsIndex
finalScore$condsIndex <- as.integer(finalScore$condsIndex)

finalScore$t_score <- as.integer(finalScore$t_score)

finalScore_list <- list(
  t_score = finalScore$t_score,
  groupIndex = finalScore$groupIndex,
  condsIndex = finalScore$condsIndex
)

#want to control for group here, and figure out why ulam doesn't work

model5 <- map2stan(
  alist(
    t_score ~ dnorm(mu, sigma),
    mu <- a + b[condsIndex] + g[groupIndex],
    a ~ dnorm(50,10),
    b[condsIndex] ~ dnorm(0,0.5),
    g[groupIndex] ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ), data = finalScore_list, chains=3)

precis(model5)
precis(model5, pars = c('b[1]', 'b[2]', 'b[3]'), depth=2)
tapply(finalScore_list$t_score, list(finalScore_list$condsIndex),mean)

post5 <- extract.samples(model5)
diff_ab_5 <- post5$b[,1] - post5$b[,2]
diff_ac_5 <- post5$b[,3] - post5$b[,2]
diff_bc_5 <- post5$b[,1] - post5$b[,3]
precis(list(diff_ab_5=diff_ab_5, diff_ac_5=diff_ac_5, diff_bc_5=diff_bc_5))



#Round 2 score: 

finalScore_R2 <- as.data.frame(finalScore_R2)

Ngroups = length(unique(finalScore_R2$u_network))
Oldgroup <- finalScore_R2$u_network
groupIndex <- array(0,length(finalScore_R2$u_network))
for (index in 1:Ngroups){
  groupIndex[Oldgroup == unique(Oldgroup)[index]] = index
}
finalScore_R2$groupIndex <- groupIndex
finalScore_R2$groupIndex <- as.integer(finalScore_R2$groupIndex)

Nconds = length(unique(finalScore_R2$condition))
Oldconds <- finalScore_R2$condition
condsIndex <- array(0,length(finalScore_R2$condition))
for (index in 1:Nconds){
  condsIndex[Oldconds == unique(Oldconds)[index]] = index
}
finalScore_R2$condsIndex <- condsIndex
finalScore_R2$condsIndex <- as.integer(finalScore_R2$condsIndex)

finalScore_R2$t_score_r2 <- as.integer(finalScore_R2$t_score_r2)

finalScore_R2_list <- list(
  t_score_r2 = finalScore_R2$t_score_r2,
  groupIndex = finalScore_R2$groupIndex,
  condsIndex = finalScore_R2$condsIndex
)

#still not sure why ulam won't work here
model5.1 <- map2stan(
  alist(
    t_score_r2 ~ dnorm(mu, sigma),
    mu <- a + b[condsIndex] + g[groupIndex],
    a ~ dnorm(30,10),
    b[condsIndex] ~ dnorm(0,0.5),
    g[groupIndex] ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ), data = finalScore_R2_list, chains=3)

precis(model5.1)
precis(model5.1, pars = c('b[1]', 'b[2]', 'b[3]'), depth=2)
tapply(finalScore_R2_list$t_score, list(finalScore_R2_list$condsIndex),mean)


post5.1 <- extract.samples(model5.1)
diff_ab_5.1 <- post5.1$b[,1] - post5.1$b[,2]
diff_ac_5.1 <- post5.1$b[,3] - post5.1$b[,2]
diff_bc_5.1 <- post5.1$b[,1] - post5.1$b[,3]
precis(list(diff_ab_5.1=diff_ab_5.1, diff_ac_5.1=diff_ac_5.1, diff_bc_5.1=diff_bc_5.1))


# EXPLORATORY: 
# model 5.2 , t_score <- t_copied

finalScoreBC <- as.data.frame(finalScoreBC)

Ngroups = length(unique(finalScoreBC$u_network))
Oldgroup <- finalScoreBC$u_network
groupIndex <- array(0,length(finalScoreBC$u_network))
for (index in 1:Ngroups){
  groupIndex[Oldgroup == unique(Oldgroup)[index]] = index
}
finalScoreBC$groupIndex <- groupIndex
finalScoreBC$groupIndex <- as.integer(finalScoreBC$groupIndex)

finalScoreBC$t_score <- as.integer(finalScoreBC$t_score)

finalScoreBC_list <- list(
  t_score = finalScoreBC$t_score,
  groupIndex = finalScoreBC$groupIndex,
  t_copied = finalScoreBC$t_copied
)


model5.2 <- map2stan(
  alist(
    t_score ~ dnorm(mu, sigma),
    mu <- a + b*t_copied + g[groupIndex],
    a ~ dnorm(50,10),
    b ~ dnorm(0,1),
    g[groupIndex] ~ dnorm(0,1),
    sigma ~ dexp(1)
  ), data = finalScoreBC_list, chains=3)

precis(model5.2)

# control condition: 

finalScoreA <- as.data.frame(finalScoreA)

Ngroups = length(unique(finalScoreA$u_network))
Oldgroup <- finalScoreA$u_network
groupIndex <- array(0,length(finalScoreA$u_network))
for (index in 1:Ngroups){
  groupIndex[Oldgroup == unique(Oldgroup)[index]] = index
}
finalScoreA$groupIndex <- groupIndex
finalScoreA$groupIndex <- as.integer(finalScoreA$groupIndex)

finalScoreA$t_score <- as.integer(finalScoreA$t_score)

finalScoreA_list <- list(
  t_score = finalScoreA$t_score,
  groupIndex = finalScoreA$groupIndex,
  t_copied = finalScoreA$t_copied
)

model5.2.1 <- map2stan(
  alist(
    t_score ~ dnorm(mu, sigma),
    mu <- a + b*t_copied + g[groupIndex],
    a ~ dnorm(50,10),
    b ~ dnorm(0,1),
    g[groupIndex] ~ dnorm(0,1),
    sigma ~ dexp(1)
  ), data = finalScoreA_list, chains=3)

precis(model5.2.1)

# EXPLORATORY: 
# model 5.3 , c_copies <- t_score


finalScoreBC <- as.data.frame(finalScoreBC)


Ngroups = length(unique(finalScoreBC$u_network))
Oldgroup <- finalScoreBC$u_network
groupIndex <- array(0,length(finalScoreBC$u_network))
for (index in 1:Ngroups){
  groupIndex[Oldgroup == unique(Oldgroup)[index]] = index
}
finalScoreBC$groupIndex <- groupIndex
finalScoreBC$groupIndex <- as.integer(finalScoreBC$groupIndex)

Nconds = length(unique(finalScoreBC$condition))
Oldconds <- finalScoreBC$condition
condsIndex <- array(0,length(finalScoreBC$condition))
for (index in 1:Nconds){
  condsIndex[Oldconds == unique(Oldconds)[index]] = index
}
finalScoreBC$condsIndex <- condsIndex
finalScoreBC$condsIndex <- as.integer(finalScoreBC$condsIndex)

finalScoreBC$t_score <- as.integer(finalScoreBC$t_score)
finalScoreBC$c_copies <- as.integer(finalScoreBC$c_copies)

finalScoreBC_list <- list(
  t_score = finalScoreBC$t_score,
  groupIndex = finalScoreBC$groupIndex,
  condsIndex = finalScoreBC$condsIndex,
  c_copies = finalScoreBC$c_copies
)


model5.3 <- map2stan(
  alist(
    c_copies ~ dnorm(mu, sigma),
    mu <- a + b*t_score + g[groupIndex],
    a ~ dnorm(6,10),
    b ~ dnorm(0,0.5),
    g[groupIndex] ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ), data = finalScoreBC_list, chains=3)

precis(model5.3)

