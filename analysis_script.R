
#### Analyses for preregistration #####
#### Run dallinger_data_cleaning.R script first"

install.packages(c("coda","mvtnorm","devtools","loo","dagitty"))
library(devtools)
devtools::install_github("rmcelreath/rethinking",ref="Experimental")
library(rethinking)

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
    a ~ dnorm(0,4),
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
    a ~ dnorm(0,4),
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
    a ~ dnorm(0,4),
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
##### Prediction 3: Participants choose to view “most copied” info more in Condition B than the other two conditions, 
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

### version with CondB as baseline:

model3.0 <- map2stan(
  alist(
    chosePrestige ~ dbinom(1, p),
    logit(p) <- a + a_p[pptIndex]*sigma_p + a_g[groupIndex]*sigma_g + b_a*CondA + b_c*CondC,
    a ~ dnorm(0,4),
    a_p[pptIndex] ~ dnorm(0,1),
    a_g[groupIndex] ~ dnorm(0,1),
    b_a ~ dnorm(0,1),
    b_c ~ dnorm(0,1),
    sigma_p ~ dcauchy(0,1),
    sigma_g ~ dcauchy(0,1)
  ),
  data=infoChosen, constraints=list(sigma_p="lower=0", sigma_g="lower=0"), 
  warmup=1000, iter=1000, chains=1, cores=1 )

precis(model3.0)
plot(precis(model3.0), pars=c("a","b_a","b_c"), labels=c("Condition C","Condition A","Condition B"))

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
mainFig <- plot(precis(model3.1, depth = 2), pars=c("b[2]","b[1]","b[3]"), labels=c("Control \n(Condition A)","Prestige \n(Condition B)","Success \n(Condition C)"), xlab="Model estimate")
title("Participants Chose Prestige")

#####
#####
##### Prediction 4: Copying rate is higher in Conditions B & C compared to Condition A because copying is only based on success in Conditions B & C
#####
#####

## Data frame is all individual answers to each question (asocialOnly) 
## which includes a column for if they chose "ask someone else" made in dallinger_data_cleaning.R

#let's try ulam from 2nd edition again: 

asocialOnly <- as.data.frame(asocialOnly)

Nppts = length(unique(asocialOnly$ppt))
Oldppt <- asocialOnly$ppt
pptIndex <- array(0,length(asocialOnly$ppt))
for (index in 1:Nppts){
  pptIndex[Oldppt == unique(Oldppt)[index]] = index
}
asocialOnly$pptIndex <- pptIndex

Ngroups = length(unique(asocialOnly$group))
Oldgroup <- asocialOnly$group
groupIndex <- array(0,length(asocialOnly$group))
for (index in 1:Ngroups){
  groupIndex[Oldgroup == unique(Oldgroup)[index]] = index
}
asocialOnly$groupIndex <- groupIndex

Nconds = length(unique(asocialOnly$condition))
Oldconds <- asocialOnly$condition
condsIndex <- array(0,length(asocialOnly$condition))
for (index in 1:Nconds){
  condsIndex[Oldconds == unique(Oldconds)[index]] = index
}
asocialOnly$condsIndex <- condsIndex


asocialOnly$copied <-as.integer(asocialOnly$copied)
asocialOnly$condsIndex <- as.integer(asocialOnly$condsIndex)
asocialOnly$pptIndex <- as.integer(asocialOnly$pptIndex)
asocialOnly$groupIndex <- as.integer(asocialOnly$groupIndex)

asocialOnly_list <- list(
  copied = asocialOnly$copied,
  pptIndex = asocialOnly$pptIndex,
  groupIndex = asocialOnly$groupIndex,
  condsIndex = asocialOnly$condsIndex )

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
  ) , data=asocialOnly_list , chains=4 , cores=4 , log_lik=TRUE )

precis(model4)


#####
#####
##### Prediction 5: Participants perform best on the quiz in Condition B & C compared to Condition A because copying is only based on success in Conditions B & C
#####

## Data frame consists of accumulated scores (including copied score) on final question
## made in dallinger_data_cleaning.R 

## try Ulam again from 2nd Edition: 

finalScore <- as.data.frame(finalScore)

Nppts = length(unique(finalScore$ppt))
Oldppt <- finalScore$ppt
pptIndex <- array(0,length(finalScore$ppt))
for (index in 1:Nppts){
  pptIndex[Oldppt == unique(Oldppt)[index]] = index
}
finalScore$pptIndex <- pptIndex
finalScore$pptIndex <- as.integer(finalScore$pptIndex)

Ngroups = length(unique(finalScore$group))
Oldgroup <- finalScore$group
groupIndex <- array(0,length(finalScore$group))
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

finalScore_list <- list(
  c_a_score = finalScore$c_a_score,
  pptIndex = finalScore$pptIndex,
  groupIndex = finalScore$groupIndex,
  condsIndex = finalScore$condsIndex
)

#the below model can't run yet as this dataset only has one condition and group in the final question
#possibly need to parameterise this based on how many divergent interations there are etc
model5 <- ulam(
  alist(
    c_a_score ~ dnorm(mu, sigma),
    mu <- a + a_p[pptIndex] + a_g[groupIndex] + b[condsIndex],
    a ~ dnorm(0,10),
    b[condsIndex] ~ dnorm(0,1),
    a_p[pptIndex] ~ dnorm(a_bar,sigma_a),
    a_g[groupIndex] ~ dnorm(0, sigma_g),
    a_bar ~ dnorm(0,10),
    sigma ~ dunif(0,10),
    sigma_a ~ dexp(1),
    sigma_g ~ dexp(1)
  ),
  data=finalScore_list, chains=4 , cores=4 , log_lik=TRUE )

