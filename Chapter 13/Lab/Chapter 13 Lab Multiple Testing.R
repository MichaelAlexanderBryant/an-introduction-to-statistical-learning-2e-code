###13.6 Lab: Multiple Testing

##13.6.1 Review of Hypothesis Tests

set.seed(6)
#100 variables each with 10 observations where the first 50 have a mean of 0.5
#and variance of 1 and the second 50 have a mean of 1 and a variance of 1
x <- matrix(rnorm(10*100), 10, 100)
x[ ,1:50] <- x[, 1:50] + 0.5

#one-sample t-test
#test H0 mu1 = 0
#set alpha = 0.05
t.test(x[,1], mu = 0)
#p-value = 0.06682 > 0.05
#therefore, don't reject the null hypothesis, but the null hypothesis is false
#since mu1 = 0.5 therefore a Type II error has occurred (failing to reject the
#null hypothesis when the null hypothesis is false)

#test 100 null hypotheses using one-sample t-test
#compute 100 p-values and set alpha = 0.05
p.values <- rep(0,100)
for (i in 1:100){
  p.values[i] <- t.test(x[,i], mu=0)$p.value
}
decision <- rep("Do not reject H0", 100)
decision[p.values <= 0.05] <- "Reject H0"

#create a 2x2 table to view results
table(decision, c(rep("H0 is False", 50), rep("H0 is True", 50)))
#W = 40 (Type II Error), U = 47, S = 10, V = 3 (Type I Error)
#alpha = 0.05 (expect to reject around 5% of true null hypotheses)
3/50 #0.06

#mean to standard deviation indicates weak signal which results in many Type II errors
0.5/1 #=0.5
#alter simulated data to have a mean:standard deviation of 1 results in less
#Type II errors
x <- matrix(rnorm(10*100), 10,100)
x[, 1:50] <- x[, 1:50] + 1
for (i in 1:100){
  p.values[i] <- t.test(x[,i], mu = 0)$p.value
}
decision <- rep("Do not reject H0", 100)
decision[p.values <= .05] <- "Reject H0"
table(decision, c(rep("H0 is False", 50), rep("H0 is True", 50)))
#Type II errors reduced from 40 to 9

##13.6.2 The Family-Wise Error Rate

#calculate FWER for m = 1:500, and alpha = 0.05, 0.01, and 0.001
m <- 1:500
fwe1 <- 1 - (1 - 0.05)^m
fwe2 <- 1 - (1 - 0.01)^m
fwe3 <- 1 - (1 - 0.001)^m

#plot FWER vs. number of hypotheses for various alpha values
par(mfrow = c(1,1))
plot(m, fwe1, type = 'l', log = "x", ylim = c(0,1), col = 2,
     ylab = "Family-Wise Error Rate", xlab = "Number of Hypotheses")
lines(m, fwe2, col = 4)
lines(m, fwe3, col = 3)
abline(h = 0.05, lty=2)
legend("topleft", c('alpha = 0.05', 'alpha = 0.01', 'alpha = 0.001'), col = c(2,4,3), lty = 1)
#FWER exceeds 0.05 for small values of m except for a low alpha values
#a lower alpha value will result in more Type II errors however (i.e., very low power)

#one-sample t-test using Fund data set
library(ISLR2)
#first five managers
fund.mini <- Fund[, 1:5]
#test on first manager
t.test(fund.mini[, 1], mu = 0)

#one-sample t-test for each of the first five managers
fund.pvalue <- rep(0,5)
for (i in 1:5){
  fund.pvalue[i] <- t.test(fund.mini[, i], mu = 0)$p.value
}
fund.pvalue
#p-values for manager 1 and 3 are lower than 0.05, but cannot reject because
#they do not take into account the multiple testing
#instead use Bonferroni's method and Holm's method to control the FWER

#adjust the p-values and reject H0 for FWER < 0.05
p.adjust(fund.pvalue, method = "bonferroni") #reject H0 for manager 1
pmin(fund.pvalue*5,1) #same as p-adjust (multiply original p-values by the number
#of null hypotheses (m) and if it is greater than 1 then the adjusted p-value is 1)
p.adjust(fund.pvalue, method = "holm") #reject H0 for managers 1 and 3

#manager 1 performs particularly well whereas manager 2 performs poorly
apply(fund.mini, 2, mean)
#is there evidence of meaningful difference in performance between the two managers?
#perform a paired t-test
t.test(fund.mini[,1], fund.mini[,2], paired = T) #p-value = 0.03839 suggesting
#statistical significance

#it was decided to perform a paired t-test only after examining the data and
#determining the highest (manager 1) and lowest (manager 2) mean performance
#implicitly performed (5 choose 2) = 5(5-1)/2 = 10 hypothesis tests rather than one
#use Tukey's method to adjust for multiple testing

#TukeyHSD requires ANOVA input
returns <- as.vector(as.matrix(fund.mini))
manager <- rep(c("1", "2", "3", "4", "5"), rep(50,5))
a1 <- aov(returns ~ manager)
TukeyHSD(x= a1) #10 hypothesis tests of every combination (5 choose 2)
#there is no longer a statistical significance between the manager's performances

#plot the confidence intervals for the pairwise comparisons
plot(TukeyHSD(x = a1))


##13.6.3 The False Discovery Rate

#perform one-sample t-tests for all 2000 fund managers where the H0,j is that mu,j = 0
#for the jth fund manager
fund.pvalues <- rep(0,2000)
for (i in 1:2000){
  fund.pvalues[i] <- t.test(Fund[,i], mu=0)$p.value
}

#too many managers to consider trying to control the FWER
#so instead use FDR
#carry out Benjamini-Hochberg prodcedure
q.values.BH <- p.adjust(fund.pvalues, method = "BH")
q.values.BH[1:10]
#q-values: smallest FDR threshold at which a particular null hypothesis can be rejected
#e.g., q-value = 0.1 indicates the null hypothesis can be rejected at an FDR of 10% or greater
#but not below 10%

#if controlling FDR at 10%, how many H0 can be rejected
sum(q.values.BH <= .1) #= 146

#146 of the 2000 fund managers beat the market at an FDR of 10%
#only 10% of 146 (about 15) will likely be false discoveries

#if Bonferroni's method is used to control FWER at alpha = 0.1 then no null hypotheses
#are rejected
sum(fund.pvalues <= (0.1/2000)) #= 0

#determine indices of p-values which result in the rejection of alternative hypotheses
#using Benjamini-Hochberg procedure
ps <- sort(fund.pvalues)
m <- length(fund.pvalues)
q <- 0.1
wh.ps <- which(ps < q * (1:m) / m)
if (length(wh.ps) > 0){
  wh <- 1:max(wh.ps)
  } else {
  wh <- numeric(0)
  }

#plot result
plot(ps, log = "xy", ylim = c(4e-6, 1), ylab = "P-Value", xlab = "Index", main = "")
points(wh, ps[wh], col = 4)
abline(a = 0, b = (q/m), col = 2, untf = TRUE)
abline(h = 0.1 /2000, col = 3)
#green line indicates the p-value threshold corresponding to FWER control via Bonferroni procedure
#at alpha = 0.1
#orange line indicates p-value thresholds corresponding to FDR control via Benjamini-Hochberg at
#q = 0.1
#blue circles are the p-values of the rejected hypotheses
#146 null hypotheses are rejected
#Benjamini-Hochberg's method is less stringent than Bonferroni's method
#demonstrates the Type-I/Type-II Error trade-off

##13.6.4 A Re-Sampling Approach

#re-sampling approach to hypothesis testing
#use Khan data set and merge training and test sets
attach(Khan)
x <- rbind(xtrain, xtest)
y <- c(as.numeric(ytrain), as.numeric(ytest))
dim(x) #83 patients, 2308 genes
table(y) #four classes of cancer

#compare the mean expressions in the 2nd and 4th classes of cancer for each gene
#two-sample t-test on the 11th gene
x <- as.matrix(x)
x1 <- x[which(y==2),]
x2 <- x[which(y==4),]
n1 <- nrow(x1)
n2 <- nrow(x2)
t.out <- t.test(x1[, 11], x2[,11], var.equal = TRUE)
TT <- t.out$statistic
TT #t-statistic = -2.093633
t.out$p.value #p-value = 0.04118644

#p-value relies on the assumption that under the null hypothesis of no difference
#between the two groups, the test statistics follows a t-distribution with 29+25-2 = 52
#degrees of freedom

#instead of using theoretical null distribution, randomly split 54 patients into two
#groups of 29 and 25 and compute new test statistic

#new test statistic should have the same distribution as the original
#repeat this process 10,000 times to approximate the null distribution of the test statistic

#compute the fraction of the time that the observed test statistic exceeds the test statistics
#obtained via re-sampling

set.seed(1)
B <- 10000
Tbs <- rep(NA, B)
for (b in 1:B){
  dat <- sample(c(x1[,11], x2[, 11]))
  Tbs[b] <- t.test(dat[1:n1], dat[(n1 + 1):(n1 + n2)],
                   var.equal = TRUE)$statistic
}
mean((abs(Tbs) >= abs(TT))) #= 0.0416 (re-sampling-based p-value)
#re-sampling-based null distribution is almost identical to the theoretical
# null distribution (original p-value = 0.04118644) 

#plot histogram of the re-sampling-based test statistics
hist(Tbs, breaks = 100, xlim = c(-4.2, 4.2), main = "",
     xlab = "Null Distribution of Test Statistic", col = 7)
lines(seq(-4.2, 4.2, len = 1000),
      dt(seq(-4.2, 4.2, len = 1000),
         df = (n1 + n2 - 2)
         )* 1000, col = 2, lwd = 3)
abline(v=TT, col = 4, lwd=2)
text(TT + 0.5, 350, paste("T= ", round(TT,4), sep = ""), col = 4)
#resampling-based null distribution (yellow)
#theoretical null distribution (red)

#implement the plug-in re-sampling FDR approach
#instead of using 2,308 genes (due to computational load) use 100 genes instead
#For each gene, first compute the observed test statistic and then produce
#10000 re-sampled test statistics
m <- 100
set.seed(1)
index <- sample(ncol(x1), m)
Ts <- rep(NA, m)
Ts.star <- matrix(NA, ncol = m, nrow = B)
for (j in 1:m){
  k <- index[j]
  Ts[j] <- t.test(x1[,k], x2[,k], var.equal = TRUE)$statistic
  for (b in 1:B){
    dat <- sample(c(x1[,k], x2[,k]))
    Ts.star[b, j] <- t.test(dat[1:n1],
                            dat[(n1 + 1):(n1 + n2)], var.equal = TRUE)$statistic
  }
}

#compute: number of rejected null hypotheses R, estimated number of false positives V,
#estimated FDR
#for a range of theshold values c
#threshold values chosen using the absolute values of the test statistics from the 100 genes
cs <- sort(abs(Ts))
FDRs <- Rs <- Vs <- rep(NA, m)
for (j in 1:m){
  R <- sum(abs(Ts) >= cs[j])
  V <- sum(abs(Ts.star) >= cs[j])/B
  Rs[j] <- R
  Vs[j] <- V
  FDRs[j] <- V/R
}
#for any given FDR, can find the genes that will be rejected
#e.g., with FDR controlled at 0.1, reject 15/100 null hypotheses
#but expect 10% of 15 to be false discoveries (1 or 2 genes)
max(Rs[FDRs <= .1]) #= 15
sort(index[abs(Ts) >= min(cs[FDRs < .1])]) #which 15 genes
max(Rs[FDRs <= .2]) #= 28 (reject 28/100 null hypotheses at FDR controlled at 0.2)
sort(index[abs(Ts) >= min(cs[FDRs < .2])]) #which 28 genes
#use index because using a subset of 100 randomly selected genes

#plot false discovery rate (FDR) against number of rejections
plot(Rs, FDRs, xlab = "Number of Rejections", type = "l",
     ylab = "False Discovery Rate", col = 4, lwd = 3)
