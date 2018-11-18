# need these packages
library(data.table)
library(survival)
library(survminer)
library(muhaz)
library(ggplot2)
library(flexsurv)
library(dplyr)


setwd("C:\\Users\\amool\\OneDrive - North Carolina State University\\Desktop\\Survival Analysis\\survivalcsv\\")

katrina <- fread('katrina.csv')

# weibull distribution
fit_wb <- flexsurvreg(Surv(hour, reason == c(2,3)) ~ backup + bridgecrane + servo + trashrack + elevation +
                        slope + age, data=katrina, dist = "weibull")
plot(fit_wb, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "cumulative hazard", main = "weibull distribution")
summary(fit_wb)

# exponential distribution
fit_exp <- flexsurvreg(Surv(hour, reason == c(2,3)) ~ backup + bridgecrane + servo + trashrack + elevation +
                         slope + age, data=katrina, dist = "exponential")
plot(fit_exp, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n", 
     xlab = "hour", ylab = "cumulative hazard", main = "exponential distribution")
summary(fit_exp)

# some other distributions: lognormal
fit_lnorm <- flexsurvreg(Surv(hour, reason == c(2,3)) ~ backup + bridgecrane + servo + trashrack + elevation +
                           slope + age, data=katrina, dist = "lognormal")
plot(fit_lnorm, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "cumulative hazard", main = "lognormal distribution")
summary(fit_lnorm)

# log-logistic
fit_llogis <- flexsurvreg(Surv(time = hour, event = reason %in% c(2,3)) ~ backup + bridgecrane + servo + trashrack + elevation +
                            slope + age, data=katrina, dist = "llogis")
plot(fit_llogis, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "cumulative hazard", main = "log-logistic distribution") #, xmax = 15)
summary(fit_llogis)

# Create AFT model
# Reason==1 since we're interested in pump failures due to flooding, using weibull distribution
katrina_fit <- survreg(Surv(time = hour, event = reason %in% c(2, 3)) ~ backup + bridgecrane + servo + trashrack + elevation +
                         slope + age, data=katrina, dist = "weibull")

summary(katrina_fit)
exp(coef(katrina_fit))

