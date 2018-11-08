# need these packages
library(data.table)
library(survival)
library(survminer)
library(muhaz)
library(ggplot2)
library(flexsurv)
library(dplyr)
library(flexsurv)

setwd("C:\\Users\\Bill\\Documents\\NCSU\\Course Work\\Fall\\Survival Analysis\\Data\\")

katrina <- fread('katrina.csv')

# weibull distribution
fit_wb <- flexsurvreg(Surv(katrina$hour, katrina$reason==1) ~ backup + bridgecrane + servo + trashrack + elevation +
                        slope + age, data=katrina, dist = "weibull")
plot(fit_wb, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "week", ylab = "cumulative hazard", main = "weibull distribution")

# exponential distribution
fit_exp <- flexsurvreg(Surv(katrina$hour, katrina$reason==1) ~ backup + bridgecrane + servo + trashrack + elevation +
                         slope + age, data=katrina, dist = "exponential")
plot(fit_exp, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n", 
     xlab = "week", ylab = "cumulative hazard", main = "exponential distribution")

# some other distributions: lognormal
fit_lnorm <- flexsurvreg(Surv(katrina$hour, katrina$reason==1) ~ backup + bridgecrane + servo + trashrack + elevation +
                           slope + age, data=katrina, dist = "lognormal")
plot(fit_lnorm, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "week", ylab = "cumulative hazard", main = "lognormal distribution")

# log-logistic
fit_llogis <- flexsurvreg(Surv(katrina$hour, katrina$reason==1) ~ backup + bridgecrane + servo + trashrack + elevation +
                            slope + age, data=katrina, dist = "llogis")
plot(fit_llogis, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "week", ylab = "cumulative hazard", main = "log-logistic distribution") #, xmax = 15)

# Create AFT model
# Reason==1 since we're interested in pump failures due to flooding, using weibull distribution
katrina_fit <- survreg(Surv(katrina$hour, katrina$reason==1) ~ backup + bridgecrane + servo + trashrack + elevation +
                         slope + age, data=katrina, dist = "weibull")

summary(katrina_fit)
exp(coef(katrina_fit))

katrina_no_upgrades <- katrina %>%
  # first, we need to get the linear predictor from the old model
  mutate(old_lp = predict(katrina_fit, type = "lp"),
         # add ID variable so we know which pumps they are
         ID = row_number()) %>%
  # next, we're only interested in those which failed by flooding
  dplyr::filter(reason == 1) %>%
  # find the survival prob at the time the pump failed
  # add columns for upgrade, new_time, new_lp, pred_time_diff
  mutate(old_time = hour,
         surv_prob = 1 - psurvreg(old_time,
                                  mean = old_lp,
                                  scale = katrina_fit$scale,
                                  distribution = katrina_fit$dist),
         upgrade = "NA", new_time = hour, new_lp = old_lp, pred_time_diff = 0)

for (i in 1:length(katrina_no_upgrades)){
  # check to see if backup can be upgraded and if it improves survival time
  if (katrina_no_upgrades[i,"backup"] == 0){
    katrina_no_upgrades[i,"backup"] = 1
    temp_new_lp = predict(katrina_fit, newdata = katrina_no_upgrades[i,], type = "lp")
    temp_new_time = qsurvreg(1 - katrina_no_upgrades$surv_prob[i], mean = temp_new_lp, scale = katrina_fit$scale, distribution = katrina_fit$dist)
    if (temp_new_time > katrina_no_upgrades[i,"new_time"]){
      katrina_no_upgrades[i,"upgrade"] = "backup"
      katrina_no_upgrades[i,"new_lp"] = temp_new_lp
      katrina_no_upgrades[i,"new_time"] = temp_new_time
      katrina_no_upgrades[i,"pred_time_diff"] = temp_new_time - katrina_no_upgrades[i,"old_time"]
    }
    katrina_no_upgrades[i,"backup"] = 0
  }
  # check to see if bridgecrane can be upgraded and if it improves survival time -> if it improves, it overwrites any previous upgrade
  if (katrina_no_upgrades[i,"bridgecrane"] == 0){
    katrina_no_upgrades[i,"bridgecrane"] = 1
    temp_new_lp = predict(katrina_fit, newdata = katrina_no_upgrades[i,], type = "lp")
    temp_new_time = qsurvreg(1 - katrina_no_upgrades$surv_prob[i], mean = temp_new_lp, scale = katrina_fit$scale, distribution = katrina_fit$dist)
    if (temp_new_time > katrina_no_upgrades[i,"new_time"]){
      katrina_no_upgrades[i,"upgrade"] = "bridgecrane"
      katrina_no_upgrades[i,"new_lp"] = temp_new_lp
      katrina_no_upgrades[i,"new_time"] = temp_new_time
      katrina_no_upgrades[i,"pred_time_diff"] = temp_new_time - katrina_no_upgrades[i,"old_time"]
    }
    katrina_no_upgrades[i,"bridgecrane"] = 0
  }
  # check to see if servo can be upgraded and if it improves survival time -> if it improves, it overwrites any previous upgrade
  if (katrina_no_upgrades[i,"servo"] == 0){
    katrina_no_upgrades[i,"servo"] = 1
    temp_new_lp = predict(katrina_fit, newdata = katrina_no_upgrades[i,], type = "lp")
    temp_new_time = qsurvreg(1 - katrina_no_upgrades$surv_prob[i], mean = temp_new_lp, scale = katrina_fit$scale, distribution = katrina_fit$dist)
    if (temp_new_time > katrina_no_upgrades[i,"new_time"]){
      katrina_no_upgrades[i,"upgrade"] = "servo"
      katrina_no_upgrades[i,"new_lp"] = temp_new_lp
      katrina_no_upgrades[i,"new_time"] = temp_new_time
      katrina_no_upgrades[i,"pred_time_diff"] = temp_new_time - katrina_no_upgrades[i,"old_time"]
    }
    katrina_no_upgrades[i,"servo"] = 0
  }
  # check to see if trashrack can be upgraded and if it improves survival time -> if it improves, it overwrites any previous upgrade
  if (katrina_no_upgrades[i,"trashrack"] == 0){
    katrina_no_upgrades[i,"trashrack"] = 1
    temp_new_lp = predict(katrina_fit, newdata = katrina_no_upgrades[i,], type = "lp")
    temp_new_time = qsurvreg(1 - katrina_no_upgrades$surv_prob[i], mean = temp_new_lp, scale = katrina_fit$scale, distribution = katrina_fit$dist)
    if (temp_new_time > katrina_no_upgrades[i,"new_time"]){
      katrina_no_upgrades[i,"upgrade"] = "trashrack"
      katrina_no_upgrades[i,"new_lp"] = temp_new_lp
      katrina_no_upgrades[i,"new_time"] = temp_new_time
      katrina_no_upgrades[i,"pred_time_diff"] = temp_new_time - katrina_no_upgrades[i,"old_time"]
    }
    katrina_no_upgrades[i,"trashrack"] = 0
  }
  # check to see if elevation can be "upgraded" and if it improves survival time -> if it improves, it overwrites any previous upgrade
  # assuming here that all pumps can have maintenance performed as an upgrade to raise by 1 ft
  katrina_no_upgrades[i,"elevation"] = katrina_no_upgrades[i,"elevation"] + 1
  temp_new_lp = predict(katrina_fit, newdata = katrina_no_upgrades[i,], type = "lp")
  temp_new_time = qsurvreg(1 - katrina_no_upgrades$surv_prob[i], mean = temp_new_lp, scale = katrina_fit$scale, distribution = katrina_fit$dist)
  if (temp_new_time > katrina_no_upgrades[i,"new_time"]){
    katrina_no_upgrades[i,"upgrade"] = "elevation"
    katrina_no_upgrades[i,"new_lp"] = temp_new_lp
    katrina_no_upgrades[i,"new_time"] = temp_new_time
    katrina_no_upgrades[i,"pred_time_diff"] = temp_new_time - katrina_no_upgrades[i,"old_time"]
  }
  katrina_no_upgrades[i,"elevation"] = katrina_no_upgrades[i,"elevation"] - 1
}

results = katrina_no_upgrades %>% arrange(desc(new_time)) %>% head(n=20) %>%
  select(ID, surv_prob, old_time, new_time, pred_time_diff, upgrade)

total_time_increase = sum(results$pred_time_diff)  #about 253 hours increase (only 19 pumps upgraded due to limit 1 per pump)
