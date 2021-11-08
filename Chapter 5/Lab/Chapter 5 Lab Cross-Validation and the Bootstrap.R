#5.3.1 The Validation Set Approach

library(ISLR2)
View(Auto)
attach(Auto)

set.seed(1)

#sample 196 from 392 observations
train <- sample(392,196)

#fit linear model
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)

#MSE
mean((mpg - predict(lm.fit, Auto))[-train]^2) #=23.27

#fit linear model with horsepower to the second power
lm.fit2 <- lm(mpg ~ poly(horsepower,2), data = Auto, subset = train)

#MSE
mean((mpg - predict(lm.fit2, Auto))[-train]^2) #=18.72

#fit linear model with horsepower to the third power
lm.fit3 <- lm(mpg ~ poly(horsepower,3), data = Auto, subset = train)

#MSE
mean((mpg - predict(lm.fit3, Auto))[-train]^2) #=18.79

#change seed to demonstrate that different training data sets results in different errors on the validation set
set.seed(2)

#sample 196 from 392 observations
train <- sample(392,196)

#fit linear model
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)

#MSE
mean((mpg - predict(lm.fit, Auto))[-train]^2) #=25.73

#fit linear model with horsepower to the second power
lm.fit2 <- lm(mpg ~ poly(horsepower,2), data = Auto, subset = train)

#MSE
mean((mpg - predict(lm.fit2, Auto))[-train]^2) #=20.43

#fit linear model with horsepower to the third power
lm.fit3 <- lm(mpg ~ poly(horsepower,3), data = Auto, subset = train)

#MSE
mean((mpg - predict(lm.fit3, Auto))[-train]^2) #=20.39

#both sets indicate that the linear function of horsepower performs worse
#than higher powers and there is little evidence that a third power is better than the second

##5.3.2 Leave-One-Out Cross-Validation

#glm function can be used to perform linear regression (like lm), just exclude "family"
glm.fit <- glm(mpg ~ horsepower, data = Auto)
coef(glm.fit)

#glm can be used with cv.glm (in boot library)
library(boot)
cv.err <- cv.glm(Auto, glm.fit)
#cross validation results (two numbers)
cv.err$delta #24.23 and 24.23 (test error = 24.23)
#the first number is the k-fold CV estimate, the second number is a bias-corrected version (same for this data set)

#array of size 10 filled with zeros
cv.error <- rep(0,10)

#create a for loop that iterates 1 through 10 for horsepower power and cross validates
for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower,i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
  }

#10 LOOCV errors
#drop in error after power=1 (24.23) then no clear improvement
cv.error

##5.3.3 k-Fold Cross-Validation

set.seed(17)
cv.error.10 <- rep(0,10)

for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  #k = 10 (10-fold cross validation)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}

#computational time is shorter for 10-fold CV than LOOCV
#still no clear evidence that using higher power than quadratic is better
cv.error.10

##5.3.4 The Bootstrap

#bootsrap is a part of the "boot" library

#function outputs an estimate of alpha (see Section 5.2)
alpha.fn <- function (data, index) {
  X <- data$X[index]
  Y <- data$Y[index]
  (var(Y) - cov(X,Y)) / (var(X) + var(Y) - 2 * cov(X,Y))
}

#use it on the portfolio data set
alpha.fn(Portfolio, 1:100)

set.seed(7)
#sample with replacement (bootstrap)
alpha.fn(Portfolio, sample(100, 100, replace = T))

#bootstrap function automates sampling Portfolio 1000 times and computes alpha.fn
#which estimates alpha and outputs the resulting standard deviation
boot(Portfolio, alpha.fn, R = 1000)

#est. alpha = 0.5758 and SE(est. alpha) = 0.0897

#create a function to estimate linear regression
boot.fn <- function(data, index){
  coef(lm(mpg ~ horsepower, data = data, subset = index))
}

#using full data set
boot.fn(Auto, 1:392) #b = 39.94, m = -0.1578

#random sampling among entire data set with replacement (bootstrap) to estimate coefficents
set.seed(1)
boot.fn(Auto, sample(392,392, replace = T)) #b = 40.34, m = -0.1635

#repeat 1000 times with boot()
boot(Auto, boot.fn, 1000)

# Bootstrap Statistics :
#     original        bias    std. error
# t1* 39.9358610  0.0549915227 0.841925746
# t2* -0.1578447 -0.0006210818 0.007348956

summary(lm(mpg ~ horsepower, data = Auto))$coef

#               Estimate  Std. Error   t value      Pr(>|t|)
# (Intercept) 39.9358610 0.717498656  55.65984 1.220362e-187
# horsepower  -0.1578447 0.006445501 -24.48914  7.031989e-81

#bootstrap standard error estimates are likely closer to the actual values
#because lm depends on assumptions which are not met since non-linearity exists
#in the data so the residuals and estimated variance will be inflated for the linear model
#this is not true for the bootstrap
#also the lm assumes that each x_i are fixed and all the variability comes from eps_i (error term)
#the bootstrap does not make this assumption

#quadratic model

boot.fn <- function(data,index) {
  coef(lm(mpg ~ horsepower + I(horsepower^2),
          data = data,
          subset = index)
       )
}

set.seed(1)
boot(Auto, boot.fn, 1000)

# Bootstrap Statistics :
#     original        bias     std. error
# t1* 56.900099702  3.511640e-02 2.0300222526
# t2* -0.466189630 -7.080834e-04 0.0324241984
# t3*  0.001230536  2.840324e-06 0.0001172164

summary(lm(mpg ~ horsepower + I(horsepower^2), data = Auto))$coef


#                 Estimate   Std. Error   t value      Pr(>|t|)
# (Intercept)     56.900099702 1.8004268063  31.60367 1.740911e-109
# horsepower      -0.466189630 0.0311246171 -14.97816  2.289429e-40
# I(horsepower^2)  0.001230536 0.0001220759  10.08009  2.196340e-21