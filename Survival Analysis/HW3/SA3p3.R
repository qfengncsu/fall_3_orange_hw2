setwd("C:\\Users\\amool\\OneDrive - North Carolina State University\\Desktop\\Survival Analysis\\survivalcsv\\")

katrina <- fread('katrina.csv') 

#subset <- katrina %>% filter(survive==0)  %>% filter(reason==1)
# have one for survival analysis anyway 
katrina$ID <- 1:nrow(katrina)
katrina <- katrina %>% filter(ID != 442, ID != 631, ID != 645)

# fit proportional hazards model using coxph()
# same structure as everything else
fit_cox <- coxph(Surv(time = hour, event = reason %in% c(2, 3)) ~ backup + bridgecrane + servo + trashrack + elevation +
               slope + age, data=katrina)
summary(fit_cox)

# plot survival curve
ggsurvplot(survfit(fit_cox), data = katrina, legend = "none", break.y.by = 0.1,
           xlab = "hour", ylab = "survival probability")
# who is this reference population?
fit_cox$means

##############
### Cox regression models part 2 ###
##########################
# just add everything below to your previous code

# shrinkage factor demonstration:
# full model
df_model <- length(fit_cox$coefficients) # number of coefficients in model
LR_model <- 2*diff(fit_cox$loglik) # LRT statistic from model
(v_full <- 1 - (df_model/LR_model)) # estimate shrinkage factor
eta_shrunk <- v_full*predict(fit_cox, newdata = katrina, type = "lp") # shrunken predictions

# fit a smaller model
#Ignore for now
fit_small <- coxph(Surv(week, arrest == 1) ~ fin + age + prio, data = recid)
df_small <- length(fit_small$coefficients)
LR_small <- 2*diff(fit_small$loglik)
(v_small <- 1 - (df_small/LR_small))
c(v_full, v_small) # smaller model estimates don't need to be shrunk as much

# see the effect of just adding a whole bunch of junk
# generate 17 random variables that i know are useless
junk <- matrix(rnorm(17*nrow(recid)), nrow(recid), 17)
fit_junk <- coxph(Surv(week, arrest == 1) ~ fin + age + race + wexp +
                    mar + paro + prio + junk, data = recid)
df_junk <- length(fit_junk$coefficients)
LR_junk <- 2*diff(fit_junk$loglik)
(v_junk <- 1 - (df_junk/LR_junk))
c(v_full, v_small, v_junk) # need to shrink the junk model estimates much more

### concordance
concordance(fit_cox)

### plot residuals
# create data frame with the event, time, martingale residuals, deviance
# residuals, and ID variable
resids <- data.frame(event = fit_cox$y[,dim(fit_cox$y)[2]],
                     time = fit_cox$y[,dim(fit_cox$y)[2] - 1],
                     res_m = residuals(fit_cox, type = "martingale"),
                     res_d = residuals(fit_cox, type = "deviance"),
                     ID = 1:length(residuals(fit_cox)))
# martingale vs. time
ggplot(resids, aes(x = time, y = res_m, color = factor(event))) +
  geom_point() +
  labs(x = "hour", y = "martingale residuals", color = "event") +
  scale_color_manual(values = c("purple", "orange"))
# deviance vs. time
ggplot(resids, aes(x = time, y = res_d, color = factor(event))) +
  geom_point() +
  labs(x = "hour", y = "deviance residuals", color = "event") +
  scale_color_manual(values = c("purple", "orange"))
# deviance vs. ID, to see which one is the largest
ggplot(resids, aes(x = ID, y = res_d, color = factor(event))) +
  geom_point() +
  labs(x = "ID", y = "deviance residuals", color = "event") +
  scale_color_manual(values = c("purple", "orange"))
# or you can just find the observation corresponding to the max deviance res.
which.max(resids$res_d) # it's observation 101

### dfbetas
ggcoxdiagnostics(fit_cox, type = "dfbetas")

### checking linearity
# age
library(visreg)
visreg(fit_cox, "age", xlab = "age", ylab = "partial residuals", gg = TRUE,
       band = FALSE) +
  geom_smooth(col = "red", fill = "red") + theme_bw()
# slope
visreg(fit_cox, "slope", xlab = "slope", ylab = "partial residuals",
       gg = TRUE, band = FALSE) +
  geom_smooth(col = "red", fill = "red") + theme_bw()
# elevation
visreg(fit_cox, "elevation", xlab = "elevation", ylab = "partial residuals",
       gg = TRUE, band = FALSE) +
  geom_smooth(col = "red", fill = "red") + theme_bw()


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

# plot survival curve
ggsurvplot(survfit(fit_cox), data = katrina, legend = "none", break.y.by = 0.1,
           xlab = "hour", ylab = "survival probability")
summary(fit_lag)




###
# i'm now making the binary variables categorical
# i don't really know why, it won't change the estimates but I guess it's
# good for you to see how to deal with them anyway?
recid <- within(recid, {
  fin <- factor(fin)
  race <- factor(race)
  wexp <- factor(wexp)
  mar <- factor(mar)
  paro <- factor(paro)
})
# refit the model
fit <- coxph(Surv(week, arrest == 1) ~ fin + age + race + wexp + mar +
               paro + prio, data = recid)

### stratification: checking PH
fit_strat <- coxph(Surv(week, arrest == 1) ~ strata(fin) + age + race + wexp +
                     mar + paro + prio, data = recid)
ggsurvplot(survfit(fit_strat), data = recid, fun = "cloglog",
           palette = c("black", "purple"), legend.labs = c("no", "yes"),
           legend.title = "financial aid", xlab = "log(week)")
summary(fit_strat)

# stratified survival curves
newdata1 <- data.frame(age = 30, race = "0", wexp = "0",
                       mar = "0", paro = "0", prio = 4)
ggsurvplot(survfit(fit_strat, newdata1), data = newdata1)
# compare to what they look like with fin as a predictor rather than strata
# notice that i included fin in newdata2 but not newdata1
newdata2 <- data.frame(fin = c("0", "1"), age = 30, race = "0", wexp = "0",
                       mar = "0", paro = "0", prio = 4)
ggsurvplot(survfit(fit, newdata2), data = newdata2,
           palette = c("black", "purple"), legend.labs = c("no", "yes"),
           legend.title = "financial aid", break.y.by = 0.1,
           xlab = "week", ylab = "survival probability",
           conf.int = FALSE)
# stratification with interactions
fit_strat2 <- coxph(Surv(week, arrest == 1) ~ age:strata(fin) + race +
                      wexp + mar + paro + prio, data = recid)
summary(fit_strat2)

### checking PH with schoenfeld residuals
# testing correlation of residuals with time
fit_zph <- cox.zph(fit, transform = "km")
fit_zph
# zph plots
plot(fit_zph, var = "fin1")
# age
plot(fit_zph, var = "age")
abline(h = 0, col = "red") # reference line at 0
abline(h = fit$coef["age"], col = "purple", lty = 2, lwd = 2) # model estimate
# trying different transformations:
# identity
fit_zph_i <- cox.zph(fit, transform = "identity")
plot(fit_zph_i, var = "age")
abline(h = 0, col = "red")
abline(h = fit$coef["age"], col = "purple", lty = 2, lwd = 2)
# log
fit_zph_log <- cox.zph(fit, transform = "log")
plot(fit_zph_log, var = "age")
abline(h = 0, col = "red")
abline(h = fit$coef["age"], col = "purple", lty = 2, lwd = 2)

### fit model with time-dependent coefficient
# let's use log(t) as an example using tt() within coxph()
# here you can define this time transform as whatever function you want
fit_tdc <- coxph(Surv(week, arrest == 1) ~ fin + race + wexp + mar + paro +
                   prio + age + tt(age), data = recid,
                 tt = function(x, time, ...){x*log(time)})
summary(fit_tdc)

### time-dependent variables
# load new datasets
recid_long <- read.csv("/your/data/folder/recid.csv", header = TRUE)
fit_long <- coxph(Surv(start, stop, arrested == 1) ~ fin + age + race + wexp +
                    mar + paro + prio + employed, data = recid_long)
summary(fit_long)
# use previous week instead
recid_lag <- read.csv("/your/data/folder/recid.csv", header = TRUE)
fit_lag <- coxph(Surv(start, stop, arrested == 1) ~ fin + age + race + wexp +
                   mar + paro + prio + employed, data = recid_lag)
summary(fit_lag)

### coxph() gives wrong r^2 using counting process:
# using the long data, i am going to make a model excluding the employed
# variable, so it's the EXACT same model we've used all along
fit2 <- coxph(Surv(start, stop, arrested == 1) ~ fin + age + race + wexp +
                mar + paro + prio, data = recid_long)
summary(fit2)
# notice that the estimates are the same, but R^2 is not!
concordance(fit2) # concordance works correctly, though
