# Orange Team 2
# Phase 3 code

library(EnvStats)
library(ks)
library(dplyr)

n=100000
prop_producing <- rep(0,n)

for (j in 1:n){

  num_wells = round(runif(n=1, min=10, max=30))

  ph = rnormTrunc(n=num_wells, mean=0.99, sd=0.05, min=0, max=1)
  pr = rnormTrunc(n=num_wells, mean=0.8, sd=0.1, min=0, max=1)
  ppw = ph*pr

  producing <- rep(0,num_wells)
  for (i in 1:num_wells){
    producing[i] <- rbinom(n=1, size=1, prob=ppw[i])
  }

  prop_producing[j] = sum(producing)/length(producing)
}

VaR5 <- quantile(prop_producing, 0.05)

prop_producing <- as.data.frame(prop_producing)
tail <- filter(prop_producing, prop_producing <= VaR5)

CVaR5 = mean(tail$prop_producing)

hist(prop_producing$prop_producing, main='Distribution of Proportion of Producing Wells', xlab='Proportion of Producing Wells')
abline(v = VaR5, col="red", lwd=2)
mtext("5% Value at Risk", at=VaR5, col="red")
abline(v = CVaR5, col="blue")

hist(rnormTrunc(n=n, mean=0.99, sd=0.05, min=0, max=1), main='Distribution of Probability of Hydrocarbons', xlab='Probability of Hydrocarbons')

hist(rnormTrunc(n=n, mean=0.8, sd=0.1, min=0, max=1), main='Distribution of Probability of Reservoir', xlab='Probability of Reservoir')
