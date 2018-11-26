# Orange Team 2
# Phase 2 code

# Load libraries
library(dplyr)
library(graphics)
library(ks)
library(EnvStats)
library(ggplot2)

# Set working directory (just add a duplicate line for yours and comment out mine)
setwd("C:\\Users\\Bill\\Documents\\NCSU\\Course Work\\Fall\\Simulation\\Data")

# Load drilling costs data
drilling_costs <- read.table("Drilling Costs.csv", header=TRUE, sep=",")
price_projections <- read.table("Price Projections.csv", header=TRUE, sep=",")


############################################################
######                  DATA PREP                     ######
############################################################
 
# Keep only 1991 to 2006 data
costs <- drilling_costs[32:47,]

# Change arithmetic returns from string to numeric
costs$Arithmetic.Return...Crude.Oil <- sub("%", "", costs$Arithmetic.Return...Crude.Oil)
costs$Arithmetic.Return...Natural.Gas <- sub("%", "", costs$Arithmetic.Return...Natural.Gas)
costs$Arithmetic.Return...Dry.Well <- sub("%", "", costs$Arithmetic.Return...Dry.Well)
costs$Arithmetic.Return...Crude.Oil <- as.numeric(costs$Arithmetic.Return...Crude.Oil)/100
costs$Arithmetic.Return...Natural.Gas <- as.numeric(costs$Arithmetic.Return...Natural.Gas)/100
costs$Arithmetic.Return...Dry.Well <- as.numeric(costs$Arithmetic.Return...Dry.Well)/100

# Re-organize the data assuming the an average cost applies to all types (using just the arithmetic changes)
avg_costs <- append(costs[,5], append(costs[,6],costs[,7]))

names(price_projections) = c("Year","High","Low","Mode")
price_projections <- price_projections[2:16,]       # Only keep price projection info from 2020-2034

############################################################
######                  KDE CREATION                  ######
############################################################

# Build a kernel density estimate of the distribution of arithmetic changes for 2006 to 2012 from the 1991 to 2006 data
Density.costs <- density(avg_costs, bw="SJ-ste")
Density.costs

Est.costs <- rkde(fhat=kde(avg_costs, h=Density.costs$bw), n=10000)  # n is how many observations do you want
hist(Est.costs, breaks=50, main='Arithmetic Change in Cost Distribution from 2006 to 2012', xlab='% Change in Cost')


############################################################
######                  SIMULATION                    ######
############################################################

# Use kernel density estimate for arithmetic changes from 2006 to 2012
# Use triangular distribution for arithmetic changes from 2012 to 2015 (avg=-9.17%, max=-22%, min=-7%)
# Use triangular distribution for arithmetic changes from 2015 to 2018 (avg=5%, max=6%, min=2%)

initial.cost.Crude <- costs[16,2]
initial.cost.Gas <- costs[16,3]
initial.cost.Dry <- costs[16,4]
initial.cost.Avg <- mean(c(initial.cost.Crude,initial.cost.Gas,initial.cost.Dry))


standardize <- function(x){
  x.std = (x - mean(x))/sd(x)
  return(x.std)
}

destandardize <- function(x.std, x){
  x.old = (x.std * sd(x)) + mean(x)
  return(x.old)
}

R <- matrix(data=cbind(1, 0.64, 0.64, 1), nrow=2)  # For IP and decline rate correlation
U <- t(chol(R))       # finds the Choleski decomposition, t is the transpose function
############################################################
n = 10000
num_wells = 1

final.cost.Crude <- rep(0,n)
final.cost.Gas <- rep(0,n)
final.cost.Dry <- rep(0,n)
final.cost.Avg <- rep(0,n)

dry_well_cost <- rep(0,n)
seismic_cost_pw <- rep(0,n)
lease_costs_pw <- rep(0,n)
overhead <- rep(0,n)
completion_costs_pw <- rep(0,n)
NPV <- rep(0,n)

for (i in 1:n){
  
  # Estimate the drilling costs for 2019
  ct1 <- initial.cost.Crude
  ct2 <- initial.cost.Gas
  ct3 <- initial.cost.Dry
  ct4 <- initial.cost.Avg
  
  r1 <- rkde(fhat=kde(avg_costs, h=Density.costs$bw), n=6)    # Changes from 2006 to 2012
  r2 <- rtri(n=3, min = -0.22, max = -0.07, mode = -0.0917)   # Changes from 2012 to 2015
  r3 <- rtri(n=4, min = 0.02, max = 0.06, mode = 0.05)        # Changes from 2015 to 2019
  
  r <- append(r1,append(r2,r3))  # Combine all arithmetic changes into single vector
  
  for (j in 1:13){
    ct1 <- ct1*(1+r[j])
    ct2 <- ct2*(1+r[j])
    ct3 <- ct3*(1+r[j])
    ct4 <- ct4*(1+r[j])
  }
  final.cost.Crude[i] <- ct1*1000
  final.cost.Gas[i] <- ct2*1000
  final.cost.Dry[i] <- ct3*1000
  final.cost.Avg[i] <- ct4*1000
  
  # Estimate the seismic costs per well
  seismic_cost_pw[i] <- rnorm(n=1, mean = 3, sd = 0.35) * 43000   # Seismic sections per well times cost per section
  
  
  # Estimate lease costs per well
  lease_costs_pw[i] <- rnorm(n=1, mean = 600, sd = 50) * 960      # Leased acres per well times cost per acre
  
  # Estimate professional overhead costs per well
  overhead[i] <- rtri(n=num_wells, min = 17200, max = 279500, mode = 215000)
  
  # Estimate completion costs
  completion_costs_pw[i] = rnorm(n=1, mean = 390000, sd = 50000)  # Completion costs per well for crude oil or natural gas
  
  ######  Dry Well Cost Estimates  ######
  #######################################
  dry_well_cost[i] <- final.cost.Avg[i] + seismic_cost_pw[i] + lease_costs_pw[i] + overhead[i]
}
  ###### Production Risk Estimates ######
  #######################################
  
  IP <- rlnorm(n=n, meanlog = 6, sdlog = .28)     # Units of barrels of oil per day (BOPD)
  DR <- runif(n=n, min = 0.15, max = 0.32)          # Decline rate
  
  Both <- cbind(standardize(DR), standardize(IP))
  Both_corr <- U %*% t(Both)                     # Correlation IP and DR     
  Both_corr <- t(Both_corr)
  
  IP <- destandardize(Both_corr[,2], IP)
  DR <- destandardize(Both_corr[,1], DR)

for (i in 1:n){
  
  Rate_yb = IP[i]
  oil_prod = rep(0,15)
  
  for (j in 1:15){
    Rate_ye = (1-DR[i]) * Rate_yb
    oil_prod[j] = 365 * (Rate_yb + Rate_ye) / 2
    Rate_yb = Rate_ye
  }

  ###### Revenue Risk Estimates ######
  ####################################
  
  # Estimate next 15 years of oil prices ($/barrel)
  oil_prices = rep(0,15)
  
  for (j in 1:15){
    oil_prices[j] <- rtri(n=1, min = price_projections$Low[j], max = price_projections$High[j], mode = price_projections$Mode[j])
  }
  
  # Estimate NRI rates (used per well, per year)
  NRI = rnorm(n=1, mean = 0.75, sd = 0.02)
  
  ###### Operatining Expenses Estimates ######
  ############################################
  
  # Estimate operatind costs: same for each well in a given year
  op_costs_pb = rep(0,15)
  
  for (j in 1:15){
    op_costs_pb[j] <- rnorm(n=1, mean = 2.25, sd = 0.3)
  }
  
  ###### NPV Calculations ######
  ##############################
  TR <- 0.046  # Tax Rate
  WACC <- 0.1  # Weighted average cost of capital
  
  x <- rep(0,15)
  for (j in 1:15){
    x[j] <- (1+WACC)^j
  }
  
  revenues <- oil_prod * oil_prices * NRI * (1 - TR)
  
  year0_costs <- final.cost.Avg[i] + seismic_cost_pw[i] + lease_costs_pw[i] + overhead[i] + completion_costs_pw[i]
  
  yearly_costs <- overhead[i] + op_costs_pb * oil_prod
  
  FNR <- revenues - yearly_costs
  
  NPV[i] = -1 * year0_costs + sum(FNR/x)
}

hist(dry_well_cost, breaks=50, main='2019 Dry Well Cost Distribution', xlab='Cost ($)')

hist(NPV, breaks=50, main='2019 NPV of 15-Year Producing Well', xlab='NPV ($)')

