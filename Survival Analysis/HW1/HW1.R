# need these packages
library(data.table)
library(survival)
library(survminer)
library(muhaz)
library(ggplot2)


katrina <- fread('Survival Analysis/Data/katrina.csv')

# create a survival curve with no stratification
# survive==0 since we're interested in pump failure (non-survival)
katrina_fit <- survfit(Surv(katrina$hour, katrina$survive==0) ~ 1, data=katrina)

# calling this gives the sample size, #events (survive==0, failure), median, and CI
katrina_fit #### do we need to stratify median by reason?
#####  sample size: 770
#####  events (pump failure): 454
#####  median: 45

# the summary gives us the time, number at risk, number of events,
summary(katrina_fit)

#plot the survival curve with no stratification
ggsurvplot(katrina_fit, data = katrina, xlab= "Time (in hours)", 
           conf.int = TRUE, palette = "#20948B",
           legend.title = "Reason for Pump Failure",
           ggtheme = theme_bw()
           )

# stratify by reason and plot
katrina_reason <- survfit(Surv(katrina$hour, katrina$survive==0) ~ katrina$reason, data = katrina, subset = katrina$reason !=0)
katrina_reason
ggsurvplot(katrina_reason, conf.int = TRUE, 
           palette=c("#F4CC70", "#DE7A22","#20948B","#A43820"),
           xlab = "Time (in hours)",
           font.x=16,
           font.y=16,
           legend.title = "Reason for Pump Failure",
           legend.labs = c('flood','motor','surge','jammed'),
           font.legend = list(size=16),
           ggtheme = theme_bw()
           )

# log-rank test
survdiff(Surv(katrina$hour, katrina$survive==0) ~ katrina$reason, rho = 0, data = katrina, subset = katrina$reason != 0)
#survdiff(Surv(katrina$hour, katrina$survive==0) ~ katrina$reason, rho = 0, data = katrina[katrina$reason != 0,]) ###strange, this gives a different output

# pairwise comparison
pairwise_survdiff(Surv(time = hour, event = survive == 0) ~ reason, rho = 0, data = katrina[katrina$reason != 0,])

# hazard functions

# here, i am making a new variable hour2, where i'm setting censored observations
# to have hour2 = 49 so that the function doesn't plot them as if they all had
# the event in the last week
katrina$hour2 <- ifelse(katrina$hour == 48 & katrina$survive == 1, 49, katrina$hour)

# create a new variable survive2 that is the opposite of survive
### we did this because kphaz.fit() automatically assumes 0 is censored 
### and 1 is the event, and it doesn't look like this can be changed.
katrina$survive2 <- ifelse(katrina$survive==0, 1, 0)

# kphaz.fit() has the same arguments as Surv()
katrina_haz <- with(katrina, kphaz.fit(hour2, survive2))

# and we plot it with kphaz.plot()
kphaz.plot(katrina_haz, main = "Hazard Function")
# to see why i needed to restructure this, look at the plot using week instead
# of week2

# extract values from katrina_haz and create better visualizaiton.
k_time <- katrina_haz$time
k_haz <- katrina_haz$haz
hazard_df <- as.data.frame(cbind(k_time, k_haz))
ggplot(data=hazard_df, y=hazard_df$k_time, x=hazard_df$k_haz)

### cumulative hazard ###
ggsurvplot(katrina_fit, fun = "cumhaz", palette = "grey")
