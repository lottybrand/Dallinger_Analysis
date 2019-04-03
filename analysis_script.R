
#### Analyses for preregistration #####
#### Run dallinger_data_cleaning.R script first"

library(rethinking)

source('dallinger_data_cleaning.R') 

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
Nppts = length(unique(scoreChoice$ppt))
Oldppt <- scoreChoice$ppt
pptIndex <- array(0,length(scoreChoice$ppt))
for (index in 1:Nppts){
  pptIndex[Oldppt == unique(Oldppt)[index]] = index
}
scoreChoice$pptIndex <- pptIndex

#make index contiguous for group varying effect:
Ngroups = length(unique(scoreChoice$group))
Oldgroup <- scoreChoice$group
groupIndex <- array(0,length(scoreChoice$group))
for (index in 1:Ngroups){
  groupIndex[Oldgroup == unique(Oldgroup)[index]] = index
}
scoreChoice$groupIndex <- groupIndex


model1 <- map2stan(
  alist(
    topCopy ~ dbinom(1, p),
    logit(p) <- a + a_p[pptIndex]*sigma_p + a_g[groupIndex]*sigma_g,
    a ~ dnorm(0,10),
    a_p[pptIndex] ~ dnorm(0,1),
    a_g[groupIndex] ~ dnorm(0,1),
    sigma_p ~ dcauchy(0,1),
    sigma_g ~ dcauchy(0,1)
  ),
  data=scoreChoice, constraints=list(sigma_p="lower=0"), 
  warmup=1000, iter=1000, chains=1, cores=1 )

precis(model1)

#####
#####
#####
##### Prediction 2: Participants copy the most-copied participant out of those available in Round 2 of Condition B
#####
#####
#####

## Dataframe is whenever someone chose to view prestige information, made in dallinger_data_cleaning.R 

prestigeChoice<- as.data.frame(prestigeChoice)

#make ppt index contiguous:
Nppts = length(unique(prestigeChoice$ppt))
Oldppt <- prestigeChoice$ppt
pptIndex <- array(0,length(prestigeChoice$ppt))
for (index in 1:Nppts){
  pptIndex[Oldppt == unique(Oldppt)[index]] = index
}
prestigeChoice$pptIndex <- pptIndex

#make group index contiguous:
Ngroups = length(unique(prestigeChoice$group))
Oldgroup <- prestigeChoice$group
groupIndex <- array(0,length(prestigeChoice$group))
for (index in 1:Ngroups){
  groupIndex[Oldgroup == unique(Oldgroup)[index]] = index
}
prestigeChoice$groupIndex <- groupIndex


model2 <- map2stan(
  alist(
    presCopy ~ dbinom(1, p),
    logit(p) <- a + a_p[pptIndex]*sigma_p + a_g[groupIndex]*sigma_g,
    a ~ dnorm(0,10),
    a_p[pptIndex] ~ dnorm(0,1),
    a_g[groupIndex] ~ dnorm(0,1),
    sigma_p ~ dcauchy(0,1),
    sigma_g ~ dcauchy(0,1)
  ),
  data=prestigeChoice, constraints=list(sigma_p="lower=0"), 
  warmup=1000, iter=1000, chains=1, cores=1 )

precis(model2)

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

Nppts = length(unique(infoChosen$ppt))
Oldppt <- infoChosen$ppt
pptIndex <- array(0,length(infoChosen$ppt))
for (index in 1:Nppts){
  pptIndex[Oldppt == unique(Oldppt)[index]] = index
}
infoChosen$pptIndex <- pptIndex

Ngroups = length(unique(infoChosen$group))
Oldgroup <- infoChosen$group
groupIndex <- array(0,length(infoChosen$group))
for (index in 1:Ngroups){
  groupIndex[Oldgroup == unique(Oldgroup)[index]] = index
}
infoChosen$groupIndex <- groupIndex

model3 <- map2stan(
  alist(
    chosePrestige ~ dbinom(1, p),
    logit(p) <- a + a_p[pptIndex]*sigma_p + a_g[groupIndex]*sigma_g + b_a*CondA + b_c*CondC,
    a ~ dnorm(0,10),
    c(b_a,b_c) ~ dnorm(0,4),
    a_p[pptIndex] ~ dnorm(0,1),
    a_g[groupIndex] ~ dnorm(0,1),
    sigma_p ~ dcauchy(0,1),
    sigma_g ~ dcauchy(0,1)
  ),
  data=infoChosen, constraints=list(sigma_p="lower=0"), 
  warmup=1000, iter=1000, chains=1, cores=1 )

precis(model3)

null_model3 <- map2stan(
  alist(
    chosePrestige ~ dbinom(1, p),
    logit(p) <- a + a_p[pptIndex]*sigma_p + a_g[groupIndex]*sigma_g,
    a ~ dnorm(0,10),
    a_p[pptIndex] ~ dnorm(0,1),
    a_g[groupIndex] ~ dnorm(0,1),
    sigma_p ~ dcauchy(0,1),
    sigma_g ~ dcauchy(0,1)
  ),
  data=infoChosen, constraints=list(sigma_p="lower=0"), 
  warmup=1000, iter=1000, chains=1, cores=1 )

#####
#####
##### Prediction 4: Copying rate is higher in Conditions B & C compared to Condition A because copying is only based on success in Conditions B & C
#####
#####

## Data frame is all individual answers to each question (asocialOnly) 
## which includes a column for if they chose "ask someone else" made in dallinger_data_cleaning.R

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


model4 <- map2stan(
  alist(
    copied ~ dbinom(1, p),
    logit(p) <- a + a_p[pptIndex]*sigma_p + a_g[groupIndex]*sigma_g + b_b*condB + b_c*condC,
    a ~ dnorm(0,10),
    c(b_b,b_c) ~ dnorm(0,4),
    a_p[pptIndex] ~ dnorm(0,1),
    a_g[groupIndex] ~ dnorm(0,1),
    sigma_p ~ dcauchy(0,1),
    sigma_g ~ dcauchy(0,1)
  ),
  data=asocialOnly, constraints=list(sigma_p="lower=0"), 
  warmup=1000, iter=1000, chains=1, cores=1 )

precis(model4)


#####
#####
##### Prediction 5: Participants perform best on the quiz in Condition B & C compared to Condition A because copying is only based on success in Conditions B & C
#####

## Data frame consists of accumulated scores (including copied score) on final question
## made in dallinger_data_cleaning.R 

finalScore <- as.data.frame(finalScore)

Nppts = length(unique(finalScore$ppt))
Oldppt <- finalScore$ppt
pptIndex <- array(0,length(finalScore$ppt))
for (index in 1:Nppts){
  pptIndex[Oldppt == unique(Oldppt)[index]] = index
}
finalScore$pptIndex <- pptIndex

Ngroups = length(unique(finalScore$group))
Oldgroup <- finalScore$group
groupIndex <- array(0,length(finalScore$group))
for (index in 1:Ngroups){
  groupIndex[Oldgroup == unique(Oldgroup)[index]] = index
}
finalScore$groupIndex <- groupIndex

model5 <- map2stan(
  alist(
    c_a_score ~ dnorm(mu, sigma),
    mu <- a + b_b*condB + b_c*condC +
      a_p[pptIndex]*pptIndex + a_g[groupIndex]*groupIndex,
    a ~ dnorm(0,10),
    c(b_b,b_c) ~ dnorm(0,4),
    a_p[pptIndex] ~ dnorm(0,1),
    a_g[groupIndex] ~ dnorm(0,1),
    sigma ~ dunif(0,10),
    sigma_p ~ dcauchy(0,1),
    sigma_g ~ dcauchy(0,1)
  ),
  data=finalScore, constraints=list(sigma_p="lower=0"),
  warmup = 1000, iter=2000, chains = 1, cores = 1)
