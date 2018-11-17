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

