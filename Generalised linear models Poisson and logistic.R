library(NHSRdatasets)
library(ggplot2)
library(ggiraphExtra)
library(dplyr)
library(tidyverse)
library(jtools)

##This dataset shows simulated length of hospital stay and Age of patients,
##as well as outcome(whether they died or not). The Dataset is from an NHS R training,
##package NHSRdatasets. The data was collected across 10 trusts, and is potentially not,
##independent (ie. length of stay(LOS), could be correlated within trusts).
##However, this first analysis assumes independence to show how to construct a generalized
##linear model with poisson and binomial outcome data.

Data <- NHSRdatasets::LOS_model

ggplot(Data, aes(Age, LOS, color)) + geom_point()

##Checking distribution of the LOS variable
qqnorm(Data$LOS) ##Not normal

hist(Data$LOS, xlab = "LOS", ylab = "frequency") ## poisson distribution 

##Checking distribution of the Age predictor 
hist(Data$Age, xlab = "Age", ylab = "frequency")## distribution of predictors,
#is not as important, we don't make assumptions about the predictors.



#POISSON REGRESSION
## Specifying poisson distribution in the model and using the log link function
Los.glm <- glm(LOS ~ Age * Death, data = Data, family = "poisson"(link = log))

## The coefficients are the log of the predicted values
summary(Los.glm)

##using effect_plot from jtools to visualise the model 
effect_plot(Los.glm, pred = Age, interval = TRUE)

effect_plot(Los.glm, pred = Death, interval = TRUE)

###LOGISTIC REGRESSION 
logglm <- glm(Death ~ Age * LOS, data = Data, family = binomial(link = "logit"))
summary(logglm)

#We can use ggPredict from the ggiraphExtra package to visualise the effect,
#of age on death across different LOS brackets
ggPredict(logglm,se=TRUE,interactive=TRUE)
