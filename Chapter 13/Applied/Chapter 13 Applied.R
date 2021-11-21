#Carseats data set
library(ISLR2)
View(Carseats)

#regress sales onto each quantitative variable (individually) and look at p-value
lm.fit1 <- lm(Sales ~ CompPrice, data = Carseats)
summary(lm.fit1) #p-value = 0.201

lm.fit2 <- lm(Sales ~ Income, data = Carseats)
summary(lm.fit2) #p-value = 0.00231

lm.fit3 <- lm(Sales ~ Advertising, data = Carseats)
summary(lm.fit3) #p-value = 4.38e-8

lm.fit4 <- lm(Sales ~ Population, data = Carseats)
summary(lm.fit4) #p-value = 0.314

lm.fit5 <- lm(Sales ~ Price, data = Carseats)
summary(lm.fit5) #p-value = <2e-16

lm.fit6 <- lm(Sales ~ Age, data = Carseats)
summary(lm.fit6) #p-value = 2.79e-6

lm.fit7 <- lm(Sales ~ Education, data = Carseats)
summary(lm.fit7) #p-value = 0.3

#control for Type-I error at level alpha = 0.05
#reject null hypotheses for Income, Advertising, Price, Age

#control FWER at level 0.05 for the p-values
1- (1-0.05)^(1/7) #alpha = 0.00730
#reject null hypotheses for Advertising, Price, Age


#control the FDR at level 0.2 for the p-values
p.values <- c(0.201, 0.00231, 4.38e-8, 0.314, 2e-16, 2.79e-6, 0.3)
q.values.BH <- p.adjust(p.values, method = "BH")
q.values.BH[q.values.BH <= 0.2]
q.values.BH
#reject null hypotheses for Income, Advertising, Price, Age

#simulate data from m=100 fund managers
set.seed(1)
n <- 20
m <- 100
X <- matrix(rnorm(n*m), ncol = m)
#data represents each fund manager's percentage returns for each of n=20 months
#test whether each fund manager's percentage returns have population mean equal to zero
#data created such that all m null hypotheses are true

#one-sample t-test for each manager
fund.pvalues <- rep(NA,100)
for (i in 1:100){
  fund.pvalues[i] <- t.test(X[,i], mu=0)$p.value
}
hist(fund.pvalues, col = 7)

length(fund.pvalues[fund.pvalues <0.05]) #erroneously reject 4 null hypotheses
#with alpha < 0.05

#control FWER at level 0.05
1-(1-0.05)^(1/100) #p-value = 0.0005128014
length(fund.pvalues[fund.pvalues < 0.0005128014]) #reject zero null hypotheses

#control FDR at level 0.05
q.values <- p.adjust(fund.pvalues, method = "BH")
length(q.values[q.values <= 0.05]) #reject zero null hypotheses

#"cherry-pick" 10 fund managers who perform best
avg.performance <- colSums(X)/20
best.managers <- sort(colSums(X)/20, decreasing = TRUE)[1:10]
top.managers <- which(avg.performance >= 0.2650217)

#control FWER at 0.05 for best managers
fund.pvalues <- rep(NA,10)
for (i in 1:10){
  fund.pvalues[i] <- t.test(X[,managers], mu=0)$p.value
}
1-(1-0.05)^(1/10) #p-value = 0.005116187
fund.pvalues[fund.pvalues < 0.005116187] #reject all null hypotheses

#control FDR at 0.05 for best managers
q.values <- p.adjust(fund.pvalues, method = "BH")
length(q.values[q.values <= 0.05]) #reject all null hypotheses

#However, selecting the top 10 managers and performing multiple testing
#is misleading, because "cherry-picking" is essentially performing multiple testing
#to choose the best managers then performing it again which does not give accurate
#results