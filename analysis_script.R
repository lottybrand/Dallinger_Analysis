
#### Analyses for preregistration #####
#### Run dallinger_data_cleaning.R script first"

library(rethinking)

#####
#####
##### Prediction 1: Participants copy the highest scoring participant out of those available in Conditions B & C
#####
#####
#####

scoreChoice<- as.data.frame(scoreChoice)

#make index contiguous for varying effects:
NOrigins = length(unique(scoreChoice$Origin))
OldOrigin <- scoreChoice$Origin
OriginIndex <- array(0,length(scoreChoice$Origin))
for (index in 1:NOrigins){
  OriginIndex[OldOrigin == unique(OldOrigin)[index]] = index
}
scoreChoice$OriginIndex <- OriginIndex


model1 <- map2stan(
  alist(
    topCopy ~ dbinom(1, p),
    logit(p) <- a + a_p[OriginIndex]*sigma_p,
    a ~ dnorm(0,10),
    a_p[OriginIndex] ~ dnorm(0,1),
    sigma_p ~ dcauchy(0,1)
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

prestigeChoice<- as.data.frame(prestigeChoice)

#make index contiguous for varying effects:
NOrigins = length(unique(prestigeChoice$Origin))
OldOrigin <- prestigeChoice$Origin
OriginIndex <- array(0,length(prestigeChoice$Origin))
for (index in 1:NOrigins){
  OriginIndex[OldOrigin == unique(OldOrigin)[index]] = index
}
prestigeChoice$OriginIndex <- OriginIndex

model2 <- map2stan(
  alist(
    presCopy ~ dbinom(1, p),
    logit(p) <- a + a_p[OriginIndex]*sigma_p,
    a ~ dnorm(0,10),
    a_p[OriginIndex] ~ dnorm(0,1),
    sigma_p ~ dcauchy(0,1)
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

#make index contiguous for varying effects:

infoChosen <- as.data.frame(infoChosen)

Nppts = length(unique(infoChosen$ppt))
Oldppt <- infoChosen$ppt
pptIndex <- array(0,length(infoChosen$ppt))
for (index in 1:Nppts){
  pptIndex[Oldppt == unique(Oldppt)[index]] = index
}
infoChosen$pptIndex <- pptIndex


model3 <- map2stan(
  alist(
    chosePrestige ~ dbinom(1, p),
    logit(p) <- a + a_p[pptIndex]*sigma_p + b_a*CondA + b_c*CondC,
    a ~ dnorm(0,10),
    c(b_a,b_c) ~ dnorm(0,4),
    a_p[pptIndex] ~ dnorm(0,1),
    sigma_p ~ dcauchy(0,1)
  ),
  data=infoChosen, constraints=list(sigma_p="lower=0"), 
  warmup=1000, iter=1000, chains=1, cores=1 )

precis(model3)

null_model3 <- map2stan(
  alist(
    chosePrestige ~ dbinom(1, p),
    logit(p) <- a + a_p[pptIndex]*sigma_p,
    a ~ dnorm(0,10),
    a_p[pptIndex] ~ dnorm(0,1),
    sigma_p ~ dcauchy(0,1)
  ),
  data=infoChosen, constraints=list(sigma_p="lower=0"), 
  warmup=1000, iter=1000, chains=1, cores=1 )

#####
#####
##### Prediction 4: Copying rate is higher in Conditions B & C compared to Condition A because copying is only based on success in Conditions B & C
#####
#####