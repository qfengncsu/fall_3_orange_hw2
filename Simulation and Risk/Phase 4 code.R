# Orange Team 2
# Phase 4 code

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
price_projections <- price_projections[2:16,]

# Get density for kernel density estimate for the distribution of arithmetic changes for 2006 to 2012 from the 1991 to 2006 data
Density.costs <- density(avg_costs, bw="SJ-ste")

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
n = 100000

NPV_project <- rep(0,n)

# Same for all wells over all iterations
TR <- 0.046  # Tax Rate
WACC <- 0.1  # Weighted average cost of captial

x <- rep(0,15)
for (k in 1:15){
  x[k] <- (1+WACC)^k
}


for (i in 1:n){
  
  num_wells = round(runif(n=1, min=10, max=30))   # Determine number of wells for a given iteration

  ph = rnormTrunc(n=num_wells, mean=0.99, sd=0.05, min=0, max=1)  # Probability of hydrocarbons
  pr = rnormTrunc(n=num_wells, mean=0.8, sd=0.1, min=0, max=1)    # Probability of reservoir
  ppw = ph*pr  # Probability of a producing well

  producing <- rep(0,num_wells)
  for (j in 1:num_wells){
    producing[j] <- rbinom(n=1, size=1, prob=ppw)   # Determine if each well is dry or producing
  }
  
  # Estimate the drilling costs for 2019 
  ct <- initial.cost.Avg
  
  r1 <- rkde(fhat=kde(avg_costs, h=Density.costs$bw), n=6)    # Changes from 2006 to 2012
  r2 <- rtri(n=3, min = -0.22, max = -0.07, mode = -0.0917)   # Changes from 2012 to 2015
  r3 <- rtri(n=4, min = 0.02, max = 0.06, mode = 0.05)        # Changes from 2015 to 2019
  
  r <- append(r1,append(r2,r3))  # Combine all arithmetic changes into single vector
  
  for (j in 1:13){
    ct <- ct * (1+r[j])
  }
  final.cost.Avg <- ct*1000    # Drilling cost per well, same for all wells in a given iteration (since average)
  
  ###### Production Risk Estimates ######
  #######################################
  
  IP <- rlnorm(n=n, meanlog = 6, sdlog = .28)     # Units of barrels of oil per day (BOPD)
  DR <- runif(n=n, min = 0.15, max = 0.32)        # Decline rate
  
  Both <- cbind(standardize(DR), standardize(IP))
  Both_corr <- U %*% t(Both)                      # Correlation IP and DR     
  Both_corr <- t(Both_corr)
  
  IP <- destandardize(Both_corr[,2], IP)
  DR <- destandardize(Both_corr[,1], DR)
  
  ###### Revenue Risk Estimates ######
  ####################################
  
  # Estimate next 15 years of oil prices ($/barrel) -> same for all wells each iteration
  oil_prices = rep(0,15)
  
  for (k in 1:15){
    oil_prices[k] <- rtri(n=1, min = price_projections$Low[k], max = price_projections$High[k], mode = price_projections$Mode[k])
  }
  
  ###### Operatining Expenses Estimates ######
  ############################################
  
  # Estimate operatind costs: same for each well in a given year
  op_costs_pb = rep(0,15)
  
  for (j in 1:15){
    op_costs_pb[j] <- rnorm(n=1, mean = 2.25, sd = 0.3)
  }

  well_value <- rep(0,num_wells)
  ###### Per Well Estimates ######
  ################################
  for (j in 1:num_wells){
    
    # Estimate the seismic costs per well (assume each well can have a different number of seismic section)
    seismic_cost_pw <- rnorm(n=1, mean = 3, sd = 0.35) * 43000   # Seismic sections per well times cost per section
  
  
    # Estimate lease costs per well (assume each well can be a different number of acres)
    lease_costs_pw <- rnorm(n=1, mean = 600, sd = 50) * 960      # Leased acres per well times cost per acre
  
    # Estimate professional overhead costs per well
    overhead <- rtri(n=num_wells, min = 17200, max = 279500, mode = 215000)
    
    if (producing[j] == 0){
      
      ######  Dry Well Cost Estimates  ######
      #######################################
      
      well_value[j] <- (final.cost.Avg + seismic_cost_pw + lease_costs_pw + overhead)*-1  # *-1 so it's a cost
    }
    else{
      
      ######  Producing Well Estimates  ######
      ########################################
      
      # Estimate completion costs
      completion_costs_pw = rnorm(n=1, mean = 390000, sd = 50000)  # Completion costs per well for crude oil or natural gas
      
      Rate_yb = IP[i]
      oil_prod = rep(0,15)
      
      for (k in 1:15){
        Rate_ye = (1-DR[i]) * Rate_yb
        oil_prod[k] = 365 * (Rate_yb + Rate_ye) / 2
        Rate_yb = Rate_ye
      }
      
      # Estimate NRI rate, different for each well, constant across years
      NRI = rnorm(n=1, mean = 0.75, sd = 0.02)
      
      revenues <- oil_prod * oil_prices * NRI * (1 - TR)
      
      year0_costs <- final.cost.Avg + seismic_cost_pw + lease_costs_pw + overhead + completion_costs_pw
      yearly_costs <- overhead + op_costs_pb * oil_prod
      
      FNR <- revenues - yearly_costs
      
      well_value[j] = -1 * year0_costs + sum(FNR/x)
    }
  }
  NPV_project[i] = sum(well_value)
}

hist(NPV_project, breaks=75, main='Project NPV Distribution', xlab='Value ($)')
range(NPV_project)
mean(NPV_project)      # This is the expected return for the project
VaR_project = quantile(NPV_project, 0.05, na.rm=T)  # Get 5% VaR
ES_project = mean(NPV_project[NPV_project < VaR_project])  # Gets the expected shortfall for 5% VaR


# Confidence Intervals for Value at Risk & Expected Shortfall - Bootstrap Approach #
n.bootstraps <- 1000
sample.size <- 1000

mean.boot <- rep(0,n.bootstraps)
VaR.boot <- rep(0,n.bootstraps)
ES.boot <- rep(0,n.bootstraps)
for(i in 1:n.bootstraps){
  bootstrap.sample <- sample(NPV_project, size=sample.size)
  mean.boot[i] <- mean(bootstrap.sample)
  VaR.boot[i] <- quantile(bootstrap.sample, VaR.percentile, na.rm=TRUE)
  ES.boot[i] <- mean(bootstrap.sample[bootstrap.sample < VaR.boot[i]], na.rm=TRUE)
}

# Get 95% Confidence Interval for Expected Return, VaR, and Expected Shortfall
ER.boot.U <- quantile(mean.boot, 0.975, na.rm=TRUE)
ER.boot.L <- quantile(mean.boot, 0.025, na.rm=TRUE)

VaR.boot.U <- quantile(VaR.boot, 0.975, na.rm=TRUE)
VaR.boot.L <- quantile(VaR.boot, 0.025, na.rm=TRUE)

ES.boot.U <- quantile(ES.boot, 0.975, na.rm=TRUE)
ES.boot.L <- quantile(ES.boot, 0.025, na.rm=TRUE)
