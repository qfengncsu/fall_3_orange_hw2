# need these packages
library(data.table)
library(survival)
library(survminer)
library(muhaz)

katrina <- fread('/Users/qing/Desktop/Survival Analysis/Data/csv/katrina.csv')

# create a survival curve with no stratification
# survive==0 since we're interested in pump failure (non-survival)
katrina_fit <- survfit(Surv(katrina$hour, katrina$survive==0) ~ 1, data=katrina)

# calling this gives the sample size, #events (survive==0, failure), median, and CI
katrina_fit
#####  sample size: 770
#####  events (pump failure): 454
#####  median: 45

# the summary gives us the time, number at risk, number of events,
summary(katrina_fit)

#plot the survival curve with no stratification
ggsurvplot(katrina_fit, data = katrina, conf.int = FALSE, palette = "grey")

# stratify by reason and plot
katrina_reason <- survfit(Surv(katrina$hour, katrina$survive==0) ~ katrina$reason, data = katrina)
katrina_reason
ggsurvplot(katrina_reason, conf.int = FALSE, color="strata")

# log-rank test
survdiff(Surv(katrina$hour, katrina$survive==0) ~ katrina$reason, rho = 0, data = katrina)

# pairwise .....