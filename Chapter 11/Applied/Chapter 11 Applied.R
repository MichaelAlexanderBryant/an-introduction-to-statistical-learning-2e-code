#brain tumor data set
library(ISLR2)
View(BrainCancer)

#plot Kaplan-Meier survival curve with +/- 1 SE bands
library(survival)
attach(BrainCancer)
fit.surv <- survfit(Surv(time,status) ~ 1, conf.int = .68)
plot(fit.surv, xlab = "Months", ylab = "Estimated Probability of Survival")

set.seed(NULL)

#initialize dataframe with time column
df <- data.frame(time = sort(BrainCancer$time))

library(dplyr)

for (i in 1:1000){
  
  bootstrap <- sample_n(BrainCancer, 88, replace = T)
  fit.bootstrap <- survfit(Surv(time,status) ~ 1,
                           data = bootstrap,
                           conf.int = .68)
  df_bootstrap<-data.frame(time = fit.bootstrap$time, fit.bootstrap$surv)
  df <- merge(x=df,y=df_bootstrap,by="time",all.x=TRUE)
}


library(zoo)

#fill with previous value (if available)
df <- na.locf(df, na.rm=FALSE)

#fill remaining na with 1.0
df[is.na(df)] <- 1.0000000

#check for null values
sum(is.na(df))

#calculate mean and SE
for (i in 1:88){
  df[i,"SE"] <- sd(as.numeric(df[i,2:201]))/sqrt(1000)
  df[i,"mean"] <- mean(as.numeric(df[i,2:201]))
}

#plot two methods for calculating SE (from model and bootstrap)
fit.surv <- survfit(Surv(time,status) ~ 1, conf.int = .68)
plot(fit.surv, xlab = "Months", ylab = "Estimated Probability of Survival")
lines(x=df[,'time'], y=df[,'mean'], col='red')
lines(x=df[,'time'], y=df[,'mean'] + df[,'SE'], col='red', lty=2)
lines(x=df[,'time'], y=df[,'mean'] - df[,'SE'], col='red', lty=2)
#the standard error bands from bootstrapping are narrower than from the model
#on the entire data set

#Cox proportional hazards model with all predictors
names(BrainCancer)
fit.cox <- coxph(Surv(time, status) ~ sex + diagnosis + loc + ki + gtv + stereo)
summary(fit.cox) #diagnosisHG glioma and ki are statistically significant

#combine 40 and 60 into bin
BrainCancer$bins <- cut(BrainCancer$ki, breaks=c(0,60,70,80,90,100), labels=c("40 or 60","70","80", "90", "100"))
BrainCancer$bins

#Kaplan-Meier survival curves stratified by ki bins
fit.ki <- survfit(Surv(time, status) ~ bins, data =BrainCancer)
plot(fit.ki, xlab = "Months", ylab = "Estimated Probability of Surival", col = c(2,3,4,5,6))
legend("bottomleft", levels(BrainCancer$bins), col = c(2,3,4,5,6), lty = 1)

#simulated data
sim_data <- data.frame(days = c(26.5, 37.2, 57.3, 90.8, 20.2, 89.8), status = c(1,1,1,0,0,0), covariate = c(0.1,11,-0.3,2.8,1.8,0.4))
#Kaplan-Meier curve
fit.surv <- survfit(Surv(days,status) ~ 1, data = sim_data)
plot(fit.surv, xlab = "Months", ylab = "Estimated Probability of Survival")

#bin the covariates into groups where X<2 and X>=2
sim_data$bin <- cut(sim_data$covariate, breaks=c(-1,2,12), labels=c('X<2', 'X>=2'))

#Kaplan-Meier of binned covariate
fit.surv <- survfit(Surv(days,status) ~ bin, data = sim_data)
plot(fit.surv, xlab = "Months", ylab = "Estimated Probability of Survival", col = c(2,4))
legend("bottomleft", levels(sim_data$bin), col=c(2,4), lty=1)
#there does appear to be some difference between the two groups

#fit a Cox's proportional hazards model
fit.cox <- coxph(Surv(days, status) ~ bin, data = sim_data)
summary(fit.cox) #est. coefficient = -0.3401
#The second group (X >= 2) has an exp(-0.3401) = 0.7117-fold increased risk that
#the event will occur (i.e., the event is less likely to occur for the second group
#compared to the first group)
#the p-value for this coefficient is 0.783 which means there is evidence for
#statistically significant difference between the two groups

#log-rank test to compare statistical significance
logrank.test <- survdiff(Surv(days, status) ~ bin, data = sim_data)
logrank.test #p-value = 0.783 which is the same as the Cox's proportional hazards model
