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

# 2. fit AFT models using survreg()
# like glm(), there's a "dist" argument (default is weibull) that we'll get to
# in a bit, but everything else works the same as you've seen before
fit.aft <- survreg(Surv(time=hour, event=reason %in% c(2,3)) ~ backup + bridgecrane + servo + trashrack + elevation +
               slope + age, data = katrina, dist = "weibull")
# and you can call a summary, which gives coefficient estimates, SEs, etc.
# notice the test for log(scale), which is testing whether log(scale) = 0,
# meaning testing if exponential is ok
summary(fit.aft)
exp(coef(fit)) # exponentiate estimates

# 3. checking assumptions
# 3.1 checking effects are constant


