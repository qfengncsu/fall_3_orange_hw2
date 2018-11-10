# Orange Team 2
# Phase 3 code

library(EnvStats)

n=10000
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

hist(prop_producing)

