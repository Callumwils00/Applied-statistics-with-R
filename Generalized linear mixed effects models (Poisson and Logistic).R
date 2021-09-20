library(NHSRdatasets)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lme4)
library(flexplot)

Data <- NHSRdatasets::LOS_model

hist(Data$LOS) ## Poisson distribution
proptab <- with(Data, table(Death)) %>% prop.table()
proptab ##17.7% die

##Poisson mixed effect model for LOS

ggplot(Data, aes(Age, LOS, color = Organisation)) + geom_point() +
  theme(legend.position = "top")

model1 <- glmer(LOS ~ Age*Death + (Age*Death | Organisation), data = Data,
                family = "poisson"(link = log), glmerControl(optimizer = "bobyqa"))

##generalised mixed effect model with random intercepts. Variables are rescaled and
##Nelder_Mead optimizer is used to help with convergence issues

sc = function(x) scale(x)[, 1]

NewData = Data %>% mutate(Age_sc = sc(Age))

model1rscale <- glmer(LOS ~ Age_sc*Death + (1 + Age_sc*Death| Organisation), data = NewData,
                      family = "poisson"(link = log), glmerControl(optimizer = "Nelder_Mead"))

## Warning "boundary (singular) fit: see ?isSingular". 

tt <- getME(model1rscale,"theta")
ll <- getME(model1rscale,"lower")
min(tt[ll==0])

## singularity is 0, the model should simplified
## Remove interactions in the random effects structure
model1rscale <- glmer(LOS ~ Age_sc*Death + (1 + Age_sc + Death| Organisation), data = NewData,
                      family = "poisson"(link = log), glmerControl(optimizer = "Nelder_Mead"))

## Remove Death from random effects
model1rscale <- glmer(LOS ~ Age_sc*Death + (1 + Age_sc | Organisation), data = NewData,
                      family = "poisson"(link = log), glmerControl(optimizer = "Nelder_Mead"))

##With only random intercepts

model1rscale <- glmer(LOS ~ Age_sc*Death + (1| Organisation), data = NewData,
                      family = "poisson"(link = log), glmerControl(optimizer = "Nelder_Mead"))

##Building a reduced model which only includes one fixed effect so that the influence of 
##fixed effects can be compared.
redmodel1 <- glmer(LOS ~ Age_sc + (1 | Organisation), data = NewData,
                   family = "poisson"(link = log), glmerControl(optimizer = "Nelder_Mead"))

compare.fits(LOS ~ Age_sc | Death, data = NewData,
             model1rscale, redmodel1) ## visualizing interaction effects. When patients die,
#there is a negative effect of age on time spent in the hospital, while there is a positive
#effect of age in general on length of stay. 

model.comparison(model1rscale, redmodel1) ## The smaller number in the aic and bic indicates 
## a better fit, larger Bayes.factor indicates better fit. Full model with both Age_sc and Death
## is favored on every stat. 


summary(model1rscale)



##Binomial mixed model for Death

ggplot(Data, aes(LOS, Death, color = Organisation)) + geom_point(position = "jitter") +
  theme(legend.position = "top")

ggplot(Data, aes(Age, Death, color = Organisation)) + geom_point(position = "jitter") +
  theme(legend.position = "top")


model2 <- glmer(Death ~ Age*LOS + (1 + Age*LOS | Organisation), data = Data,
                family = binomial, glmerControl(optimizer = "bobyqa"))

##Failed to converge, rerun the model with centered data and Nelder_Mead optimizer
NewData2 <- NewData %>% mutate(LOS_sc = sc(LOS))

model2 <- glmer(Death ~ Age_sc*LOS_sc + (1 + Age_sc*LOS_sc | Organisation), data = NewData2,
                family = binomial, glmerControl(optimizer = "Nelder_Mead"))

## Simplify the model
model2 <- glmer(Death ~ Age_sc*LOS_sc + (1 + Age_sc + LOS_sc | Organisation), data = NewData2,
                family = binomial, glmerControl(optimizer = "Nelder_Mead"))

##Warning "boundary (singular) fit: see ?isSingular"
tt <- getME(model2,"theta")
ll <- getME(model2,"lower")
min(tt[ll==0]) ## Singularity is 0

model2 <- glmer(Death ~ Age_sc*LOS_sc + (1 + Age_sc | Organisation), data = NewData2,
                family = binomial, glmerControl(optimizer = "Nelder_Mead"))

##Just using random intercepts
model2 <- glmer(Death ~ Age_sc*LOS_sc + (1 | Organisation), data = NewData2,
                family = binomial, glmerControl(optimizer = "Nelder_Mead"))

redmodel2 <- glmer(Death ~ Age_sc + (1 | Organisation), data = NewData2,
                   family = binomial, glmerControl(optimizer = "Nelder_Mead"))

compare.fits(Death ~ Age_sc | LOS_sc, data = NewData2,
             model2, redmodel2) ## There is an interaction effect, as length of stay increases,
#there is a negative effect of age on Death, this ay be because the older patients just stayed
#longer in the hospital regardless of outcome

model.comparison(model1rscale, redmodel1)## the full model is a better fit.

summary(model2)

  