# Orange Team 2
# Phase 1 code

# Load libraries
library(dplyr)
library(graphics)
library(ks)
library(EnvStats)
library(ggplot2)

# Set working directory (just add a duplicate line for yours and comment out mine)
setwd("C:\\Users\\Bill\\Documents\\NCSU\\Course Work\\Fall\\Simulation\\Data")

# Load drilling costs data
data <- read.table("Drilling Costs.csv", header=TRUE, sep=",")

############################################################
######                  DATA PREP                     ######
############################################################
 
# Keep only 1991 to 2006 data
costs <- data[32:47,]

# Change arithmetic returns from string to numeric
costs$Arithmetic.Return...Crude.Oil <- sub("%", "", costs$Arithmetic.Return...Crude.Oil)
costs$Arithmetic.Return...Natural.Gas <- sub("%", "", costs$Arithmetic.Return...Natural.Gas)
costs$Arithmetic.Return...Dry.Well <- sub("%", "", costs$Arithmetic.Return...Dry.Well)
costs$Arithmetic.Return...Crude.Oil <- as.numeric(costs$Arithmetic.Return...Crude.Oil)/100
costs$Arithmetic.Return...Natural.Gas <- as.numeric(costs$Arithmetic.Return...Natural.Gas)/100
costs$Arithmetic.Return...Dry.Well <- as.numeric(costs$Arithmetic.Return...Dry.Well)/100

# Re-organize the data assuming the an average cost applies to all types (using just the arithmetic changes)
avg_costs <- append(costs[,5], append(costs[,6],costs[,7]))

# Create histogram of arithmetic cost changes from 1991 to 2006
hist(avg_costs, breaks=10, main='Arithmetic Change in Cost Distribution', xlab='% Change in Cost')

############################################################
######                  KDE CREATION                  ######
############################################################

# Build a kernel density estimate of the distribution of arithmetic changes for 2006 to 2012 from the 1991 to 2006 data
Density.costs <- density(avg_costs, bw="SJ-ste")
Density.costs

Est.costs <- rkde(fhat=kde(avg_costs, h=Density.costs$bw), n=10000)  # n is how many observations do you want
hist(Est.costs, breaks=50, main='Arithmetic Change in Cost Distribution from 2006 to 2012', xlab='% Change in Cost')

# Check estimated distribution to see if it comes out normal
qqnorm(Est.costs)

############################################################
######                  SIMULATIONS                   ######
############################################################

# Use triangular distribution for arithmetic changes from 2012 to 2015 (avg=-9.17%, max=-22%, min=-7%)
# Use triangular distribution for arithmetic changes from 2015 to 2018 (avg=5%, max=6%, min=2%)

initial.cost.Crude <- costs[16,2]
initial.cost.Gas <- costs[16,3]
initial.cost.Dry <- costs[16,4]
initial.cost.Avg <- mean(c(initial.cost.Crude,initial.cost.Gas,initial.cost.Dry))

############################################################
# Simulation 1: Assume normal distribution for 2006 to 2012
num <- 100000
final.cost.Crude <- rep(0,num)
final.cost.Gas <- rep(0,num)
final.cost.Dry <- rep(0,num)
final.cost.Avg <- rep(0,num)

for(i in 1:num){
  ct1 <- initial.cost.Crude
  ct2 <- initial.cost.Gas
  ct3 <- initial.cost.Dry
  ct4 <- initial.cost.Avg
  
  r1 <- rnorm(n=6, mean=mean(avg_costs), sd=sd(avg_costs))    # Changes from 2006 to 2012
  r2 <- rtri(n=3, min = -0.22, max = -0.07, mode = -0.0917)   # Changes from 2012 to 2015
  r3 <- rtri(n=4, min = 0.02, max = 0.06, mode = 0.05)        # Changes from 2015 to 2019
  
  r <- append(r1,append(r2,r3))  # Combine all arithmetic changes into single vector
  
  for(j in 1:13){
    ct1 <- ct1*(1+r[j])
    ct2 <- ct2*(1+r[j])
    ct3 <- ct3*(1+r[j])
    ct4 <- ct4*(1+r[j])
  }
  final.cost.Crude[i] <- ct1
  final.cost.Gas[i] <- ct2
  final.cost.Dry[i] <- ct3
  final.cost.Avg[i] <- ct4
}

# Plot cost histograms from simuulation 1
hist(final.cost.Crude, breaks=50, main='2019 Crude Oil Well Cost Distribution', xlab='Cost ($1000/well')
abline(v = initial.cost.Crude, col="red", lwd=2)
mtext("2006 Cost", at=initial.cost.Crude, col="red")

hist(final.cost.Gas, breaks=50, main='2019 Natural Gas Well Cost Distribution', xlab='Cost ($1000/well')
abline(v = initial.cost.Gas, col="red", lwd=2)
mtext("2006 Cost", at=initial.cost.Gas, col="red")

hist(final.cost.Dry, breaks=50, main='2019 Dry Well Cost Distribution', xlab='Cost ($1000/well')
abline(v = initial.cost.Dry, col="red", lwd=2)
mtext("2006 Cost", at=initial.cost.Dry, col="red")

hist(final.cost.Avg, breaks=50, main='2019 Average Well Cost Distribution', xlab='Cost ($1000/well')
abline(v = initial.cost.Avg, col="red", lwd=2)
mtext("2006 Cost", at=initial.cost.Avg, col="red")

############################################################
# Simulation 2: Use kernel density estimate for 2006 to 2012
final.cost.Crude2 <- rep(0,num)
final.cost.Gas2 <- rep(0,num)
final.cost.Dry2 <- rep(0,num)
final.cost.Avg2 <- rep(0,num)

for(i in 1:num){
  ct1 <- initial.cost.Crude
  ct2 <- initial.cost.Gas
  ct3 <- initial.cost.Dry
  ct4 <- initial.cost.Avg
  
  r1 <- rkde(fhat=kde(avg_costs, h=Density.costs$bw), n=6)    # Changes from 2006 to 2012
  r2 <- rtri(n=3, min = -0.22, max = -0.07, mode = -0.0917)   # Changes from 2012 to 2015
  r3 <- rtri(n=4, min = 0.02, max = 0.06, mode = 0.05)        # Changes from 2015 to 2019
  
  r <- append(r1,append(r2,r3))  # Combine all arithmetic changes into single vector
  
  for(j in 1:13){
    ct1 <- ct1*(1+r[j])
    ct2 <- ct2*(1+r[j])
    ct3 <- ct3*(1+r[j])
    ct4 <- ct4*(1+r[j])
  }
  final.cost.Crude2[i] <- ct1
  final.cost.Gas2[i] <- ct2
  final.cost.Dry2[i] <- ct3
  final.cost.Avg2[i] <- ct4
}

# Plot cost histograms from simulation 2
hist(final.cost.Crude2, breaks=50, main='2019 Crude Oil Well Cost Distribution', xlab='Cost ($1000/well')
abline(v = initial.cost.Crude, col="red", lwd=2)
mtext("2006 Cost", at=initial.cost.Crude, col="red")

hist(final.cost.Gas2, breaks=50, main='2019 Natural Gas Well Cost Distribution', xlab='Cost ($1000/well')
abline(v = initial.cost.Gas, col="red", lwd=2)
mtext("2006 Cost", at=initial.cost.Gas, col="red")

hist(final.cost.Dry2, breaks=50, main='2019 Dry Well Cost Distribution', xlab='Cost ($1000/well')
abline(v = initial.cost.Dry, col="red", lwd=2)
mtext("2006 Cost", at=initial.cost.Dry, col="red")

hist(final.cost.Avg2, breaks=50, main='2019 Average Well Cost Distribution', xlab='Cost ($1000/well')
abline(v = initial.cost.Avg, col="red", lwd=2)
mtext("2006 Cost", at=initial.cost.Avg, col="red")

############################################################
######              COMBINED PLOTS                    ######
############################################################

df=data.frame(final.cost.Avg, final.cost.Crude, final.cost.Gas, final.cost.Dry,
              final.cost.Avg2, final.cost.Crude2, final.cost.Gas2, final.cost.Dry2)

# Get mean, median, and mode for each data set
stats = matrix(data=rep(0, 32), nrow=4, ncol=8)
for (i in 1:8) {
  stats[1,i] = mean(df[,i])
  stats[2,i] = median(df[,i])
  stats[3,i] = quantile(df[,i], 0.05)
  stats[4,i] = quantile(df[,i], 0.95)
}


ggplot(data=df) + geom_density(aes(x=final.cost.Avg), fill="blue", alpha=0.25) +
                  geom_density(aes(x=final.cost.Avg2), fill="red", alpha=0.25) +
                  geom_vline(aes(xintercept = initial.cost.Avg), colour="black", linetype="dashed") +
                  geom_text(aes(x=initial.cost.Avg, y=0.00033), label="2006 Cost")

ggplot(data=df) + geom_histogram(aes(x=final.cost.Avg)) +
  geom_vline(aes(xintercept = initial.cost.Avg), colour="black", linetype="dashed") +
  geom_text(aes(x=initial.cost.Avg, y=17500), label="2006 Cost")

ggplot(data=df) + geom_density(aes(x=final.cost.Avg), colour="blue", alpha=0.5) +
                  geom_density(aes(x=final.cost.Crude), colour="red", alpha=0.5) +
                  geom_density(aes(x=final.cost.Gas), colour="green", alpha=0.5) +
                  geom_density(aes(x=final.cost.Dry), colour="orange" ,alpha=0.5)
