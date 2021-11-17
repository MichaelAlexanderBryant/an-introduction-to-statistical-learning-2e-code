###11.8 Lab: Survival Analysis

##11.8.1 Brain Cancer Data

library(ISLR2)
names(BrainCancer)
attach(BrainCancer)
table(sex)
table(diagnosis)
table(status) #35 died before the end of the study

#Kaplan-Meier survival curve
library(survival)
fit.surv <- survfit(Surv(time,status) ~ 1)
plot(fit.surv, xlab = "Months", ylab = "Estimated Probability of Survival")

#Kaplan-Meier survival curves stratified by sex
fit.sex <- survfit(Surv(time, status) ~ sex)
plot(fit.sex, xlab = "Months", ylab = "Estimated Probability of Surival", col = c(2,4))
legend("bottomleft", levels(sex), col = c(2,4), lty = 1)

#log-rank test to compare survival of males and females
logrank.test <- survdiff(Surv(time, status) ~ sex)
logrank.test #p-value = 0.2 means no evidence of difference in survival between the two sexes

#Cox proportional hazards models
#model that uses sex as only predictor
fit.cox <- coxph(Surv(time, status) ~ sex)
summary(fit.cox)
#unrounded values from Cox proportional hazard model
summary(fit.cox)$logtest[1]
summary(fit.cox)$waldtest[1]
summary(fit.cox)$sctest[1]
#no clear evidence for a difference in survival between males and females for any test
logrank.test$chisq
#Cox model is exactly equal to the log-rank test statistic so these results are expected

#model using multiple predictors
fit.all <- coxph(Surv(time, status) ~ sex + diagnosis + loc + ki + gtv + stereo)
fit.all
#diagnosis has been coded such that the baseline corresponds to meningioma
#results: risk associated with HG glioma is more than eight times (exp(2.15)=8.62)
#the risk associated with meningioma (i.e., HG glioma has much worse survival)
#larget Karnofsky index associated with lower risk (i.e., longer survival)

#plot curve for each diagnosis category adjusting for the other predictors
modaldata <- data.frame(
  diagnosis = levels(diagnosis),
  sex = rep("Female", 4),
  loc = rep("Supratentorial", 4),
  ki = rep(mean(ki), 4),
  gtv = rep(mean(gtv), 4),
  stereo = rep("SRT", 4)
)
View(modaldata)

survplots <- survfit(fit.all, newdata = modaldata)
plot(survplots, xlab = "Months",
     ylab= "Survival Probability", col = 2:5)
legend("bottomleft", levels(diagnosis), col = 2:5, lty = 1)


##11.8.2 Publication Data

#stratified Kaplan-Meier curves on positive result variable
fit.posres <- survfit(Surv(time, status) ~ posres, data = Publication)
plot(fit.posres, xlab="Months", ylab="Probability of Not Being Published", col = 3:4)
legend("topright", c("Negative Result", "Positive Result"), col = 3:4, lty = 1)

#p-values are large indicating no evidence of a difference in time-to-publish between positive and negative results
fit.pub <- coxph(Surv(time, status) ~ posres, data = Publication)
fit.pub #p-value = 0.36

#log-rank test provides an identical conclusion
logrank.test <- survdiff(Surv(time, status) ~ posres, data = Publication)
logrank.test #p-value = 0.36

#results change when other predictors are added to the model
#Cox proportional hazard model
fit.pub2 <- coxph(Surv(time,status) ~ . -mech, data = Publication) #not including funding mechanism variable
fit.pub2 #statistically significant variables: positive result, clinical endpoint, and impact of the study

##11.8.3 Call Center Data

#simulated data, observed wait times (in seconds) for 2000 customers who called the call center
#censoring occurs if the customer hangs up before his or her call is answered
#covariates: operators (number of operators from 5 to 15 available at the time of the call),
#center (A, B, or C), and time of day (morning, afternoon, or evening)
#generated such that all possibilities are equally likely

set.seed(4)
N <- 2000
Operators <- sample(5:15, N, replace = T)
Center <- sample(c('A','B','C'), N, replace = T)
Time <- sample(c('Morn.', 'After.', 'Even.'), N, replace = T)
X <- model.matrix( ~ Operators + Center + Time)[, -1]

#look at design matrix (first five entries) for coding
X[1:5, ]

#specify the coefficients and the hazard function
true.beta <- c(0.04, -0.3, 0, 0.2, -0.2)
h.fn <- function(x) return(0.00001*x)

#coefficient associated with operators = 0.04
#each additional operator is associated with an exp(0.04) = 1.041-fold increased risk
#that the call will be answered given center and time
#coefficient for center B is -0.3 and A is the baseline
#exp(-0.3) = 0.74-fold increased risk of being answered at B than A (longer wait time at B)

#generate data where maximum possible wait time for a customer is 1000 seconds
install.packages('coxed')
library(coxed)
queuing <- sim.survdata(N = N, T = 1000, X = X, beta = true.beta, hazard.fun = h.fn)
names(queuing)
head(queuing$data)
#failed = T means the call was answered, failed = F means the customer hung up
mean(queuing$data$failed) #=89% of calls were answered

#Kaplan-Meier survival curve stratified by center
par(mfrow=c(1,2))
fit.Center <- survfit(Surv(y, failed) ~ Center, data = queuing$data)
plot(fit.Center, xlab = "Seconds", ylab= "Probability of Still Being on Hold", col = c(2,4,5))
legend("topright", c("Call Center A", "Call Center B", "Call Center C"), col = c(2,4,5), lty = 1)

#Kaplan-Meier survival curve stratified by time
fit.Time <- survfit(Surv(y, failed) ~ Time, data = queuing$data)
plot(fit.Time, xlab = "Seconds", ylab= "Probability of Still Being on Hold", col = c(2,4,5))
legend("topright", c("Morning", "Afternoon", "Evening"), col = c(5,2,4), lty = 1)

#Call Center B takes longer than A and C, wait times are longest in the morning and shortest in the evening
#use log-rank test to determine whether differences are statistically significant
survdiff(Surv(y,failed) ~ Center, data = queuing$data)
survdiff(Surv(y,failed) ~ Time, data = queuing$data)
#differences are highly significant (p-vaues for chi-squared tests: 7e-7 and 7e-11, respectively)

#Cox's proportional hazards model
fit.queuing <- coxph(Surv(y,failed) ~ ., data = queuing$data)
fit.queuing
#p-values for center B, evening, and morning are very small
#the hazard (i.e., instantaneous risk that a call will be answered) increases with the number of operators
#comparing estimated coefficients (0.041, -0.21, 0.079, 0.20, -0.17) with true coefficients (0.04, -0.3, 0, 0.2, -0.2)
#the Cox model is fairly accurate