## Hypothesis testing using bootstrapping

library(datasets)
library(ggplot2)
library(dplyr)

Data <- datasets::iris
Data <- Data[Data$Species %in% c("setosa", "versicolor"), ]

ggplot(Data, aes(Species, Sepal.Length)) + geom_violin(trim = FALSE) +
  geom_jitter(shape=16, position=position_jitter(0.2))

table(Data$Species)

## Null Hypothesis: there is no effect of Species on sepal length

## Using mean and median as test statistic. The distribution of the test statistic is
## found by using bootstrapping. 

##mean 
means <- Data %>% group_by(Species) %>% 
  summarise_at(vars(Sepal.Length), list(Speciesmean = mean))

tstat <- abs(means$Speciesmean[[1]] - means$Speciesmean[[2]])



##median
medians <- Data %>% group_by(Species) %>% 
  summarise_at(vars(Sepal.Length), list(Speciesmedian = median))

tstat2 <- abs(medians$Speciesmedian[[1]] - medians$Speciesmedian[[2]])

## A p value quantifies how likely we are to find a tstatistic of 0.93 or 0.9
## respectively is our Null hypothesis is true (ie. that tstat, and tstat2 are 0)


## Bootstrapping randomly assigns with replacement 50 observations from the collected
## Data to each species. This treats the data as though it all comes 
## from the same species. 
## P value = (Number of times the Bootstrapped tstat >= observed tstat)/Number of Bootstraps

set.seed(100)
n <- length(Data$Species)
B <- 1000000
variable <- Data$Sepal.Length

## create a matrix of randomly assigned values
Bootstrapsamp <- matrix(sample(variable, size = n*B, replace = TRUE),
                        nrow = n, ncol = B)

## creating vectors of 10000 0s for each test statistic
Boot.test.tstat <- rep(0, B)
Boot.test.tstat2 <- rep(0, B)

for(i in 1:B){
  Boot.test.tstat[i] <- abs(mean(Bootstrapsamp[1:50, i])-mean(Bootstrapsamp[51:100, i]))
  Boot.test.tstat2[i] <- abs(median(Bootstrapsamp[1:50, i]) - median(Bootstrapsamp[51:100, i])) 
}

## View first 25 bootstrapped test statistics

round(Boot.test.tstat[1:25], 1) ## remember tstat is 0.93
round(Boot.test.tstat2[1:25], 1) ## tstat2 is 0.9

(Boot.test.tstat >= tstat)[1:25]
(Boot.test.tstat2 >= tstat2)[1:25]

## P value
mean(Boot.test.tstat >= tstat) ## p = 0
mean(Boot.test.tstat2 >= tstat2) ## p = 3e-06 or 0.000003

## the difference between species Sepal lengths is statistically significant when using
## mean or median 



## Building the confidence intervals using bootstrapping

set.seed(100)
n.s <- 50
n.v <- 50
B <- 1000000

Bootstrappedsetosa <- matrix(sample(variable[Data$Species == "setosa"], size = B*n.s,
                                    replace = TRUE), ncol = B, nrow = n.s)

Bootstrappedversicolor <- matrix(sample(variable[Data$Species == "versicolor"], size = B*n.v,
                                        replace = TRUE), ncol = B, nrow = n.s)

dim(Bootstrappedsetosa)
dim(Bootstrappedversicolor)


Bootdiffermeans <- colMeans(Bootstrappedsetosa) - colMeans(Bootstrappedversicolor)
Bootdiffermeans

length(Bootdiffermeans)

Bootdiffermedians <- apply(Bootstrappedsetosa, MARGIN = 2, FUN = median) -
  apply(Bootstrappedversicolor, MARGIN = 2, FUN = median)
Bootdiffermedians

length(Bootdiffermedians)


## Creating 95% confidence interval 

quantile(Bootdiffermeans, prob = 0.025)
quantile(Bootdiffermeans, prob = 0.975)
## we can be 95% sure that the mean Sepal length for setosa is between 1.102 and 0.758 shorter than
## versicolor

quantile(Bootdiffermedians, prob = 0.025)
quantile(Bootdiffermedians, prob = 0.975)
## we can be 95% sure that the median Sepal length for setosa is between 1.1 and 0.65 shorter than versicolor


