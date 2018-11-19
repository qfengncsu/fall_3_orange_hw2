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




# need these packages
library(survival)
library(survminer)
library(visreg)
library(ggplot2)

# load data
katrina <- read.csv("/Users/qing/Desktop/Survival Analysis/Data/csv/katrina.csv", header = TRUE)

# create an ID variable: we'll need it later and you should probably always
# have one for survival analysis anyway 
katrina$ID <- 1:nrow(katrina)

# exclude ID= 442, 631, 645
katrina <- katrina[!(katrina$ID=='442'| katrina$ID=="631" | katrina$ID=="645"),]

# 1. fit proportional hazards model using coxph()
# same structure as everything else
# model motor and surge failures together and treat all other failure reasons as censored.
fit.cox <- coxph(Surv(time=hour, event=reason %in% c(2,3)) ~ backup + bridgecrane + servo + trashrack + elevation +
                   slope + age, data = katrina)
summary(fit.cox)
sink("output.txt")
print(summary(fit.cox))

### checking constant effect
fit_backup <- coxph(Surv(time=hour, event=reason %in% c(2,3)) ~ strata(backup) + bridgecrane + servo + trashrack + elevation +
                      slope + age, data = katrina)
ggsurvplot(survfit(fit_backup), data = katrina, fun = "cloglog",
           palette = c("#36688D", "#F18904"), legend.labs = c("no", "yes"),
           legend.title = "Backup Pump", xlab = "log(hour)")

fit_bridgecrane <- coxph(Surv(time=hour, event=reason %in% c(2,3)) ~ backup + strata(bridgecrane) + servo + trashrack + elevation +
                           slope + age, data = katrina)
ggsurvplot(survfit(fit_bridgecrane), data = katrina, fun = "cloglog",
           palette = c("#36688D", "#F18904"), legend.labs = c("no", "yes"),
           legend.title = "Bridge Crane", xlab = "log(hour)")

fit_servo <- coxph(Surv(time=hour, event=reason %in% c(2,3)) ~ backup + bridgecrane + strata(servo) + trashrack + elevation +
                     slope + age, data = katrina)
ggsurvplot(survfit(fit_servo), data = katrina, fun = "cloglog",
           palette = c("#36688D", "#F18904"), legend.labs = c("no", "yes"),
           legend.title = "Servo", xlab = "log(hour)")

fit_trashrack <- coxph(Surv(time=hour, event=reason %in% c(2,3)) ~ backup + bridgecrane + servo + strata(trashrack) + elevation +
                         slope + age, data = katrina)
ggsurvplot(survfit(fit_trashrack), data = katrina, fun = "cloglog",
           palette = c("#36688D", "#F18904"), legend.labs = c("no", "yes"),
           legend.title = "Trashrack Clenaer", xlab = "log(hour)")



setwd("C:\\Users\\amool\\OneDrive - North Carolina State University\\Desktop\\Survival Analysis\\survivalcsv\\")

katrina <- fread('katrina.csv') 

#subset <- katrina %>% filter(survive==0)  %>% filter(reason==1)
# have one for survival analysis anyway 
katrina$ID <- 1:nrow(katrina)
katrina <- katrina %>% filter(ID != 442, ID != 631, ID != 645)


#Convert to long

katrina_long = reshape(katrina, direction = "long", varying = list(names(katrina)[9:57]), v.names = "Running", 
                       idvar = c("ID","hour"), timevar = "start", times = 0:48)
katrina_long <- katrina_long %>% filter(start!=48)

katrina_long <- katrina_long[order(katrina_long$ID),] 

#katrina_long$start = katrina_long$hour
katrina_long$stop <- katrina_long$start + 1

#katrina2 <- data.frame(cbind(katrina$ID, katrina$hour))
#colnames(katrina2) <- c("ID", "hour_fail")

#katrina_long2 <- merge(x = katrina_long, y = katrina2, by = c("ID"), all.x = TRUE)
katrina_long$Running <- ifelse(katrina_long$hour<=katrina_long$start, NA, katrina_long$Running)

katrina_long2<-katrina_long[-which(is.na(katrina_long$Running)),]

#Remove 3 pumps that Bill said

#katrina_long2$reason1 <- ifelse(katrina_long2$hour!=katrina_long2$hour_fail, 0, katrina_long2$reason)
#katrina_long2$hour_fail <- ifelse(katrina_long2$hour!=katrina_long2$hour_fail, 0, katrina_long2$hour_fail)

library(data.table)
#katrina_long2$count <- sequence(rle(as.character(katrina_long$Running))$lengths)
setDT(katrina_long2)[, LargestRunning := seq(.N) * Running, by = .(ID, rleid(Running))]

katrina_long2$twelve <- ifelse(katrina_long2$LargestRunning>11, 1, 0)

#Shift
katrina_long2$twelve <- shift(katrina_long2$twelve, n=1L, fill=0, type=c("lag", "lead"), give.names=FALSE)

#Fill hours 0 to 12 with NA
katrina_long2$twelve <- ifelse(katrina_long2$start<12, NA, katrina_long2$twelve)




#Fit on long
#X matrix deemed to be singular
fit_coxlong <- coxph(Surv(start, stop, event = reason %in% c(2, 3)) ~ twelve + backup + bridgecrane + servo + trashrack + elevation +
                       slope + age, data=katrina_long2)
summary(fit_cox12)
