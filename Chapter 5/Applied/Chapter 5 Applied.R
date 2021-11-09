set.seed(1)

library(ISLR2)

#Default data set
View(Default)
names(Default)
attach(Default)

#logistic regression
lr.fit <- glm(default ~ income + balance,
              family = binomial,
              data = Default)

length(default)

#cv with four different sets

#cv1
data <- 1:10000
test <- 1:2000
train <- data[data != test]
test <- Default[1:2000,]
test.target <- default[1:2000]

lr_vs.fit <- glm(default ~ income + balance,
                 family = binomial,
                 data = Default,
                 subset= train)

lr_vs.prob <- predict(lr_vs.fit, test, type="response")

lr_vs.pred <- rep("No", 2000)

lr_vs.pred[lr_vs.prob > .5] <- "Yes"

table(lr_vs.pred, test.target)

1- mean(lr_vs.pred == test.target) #2.75% error rate

#cv2
data <- 1:10000
test <- 2001:4000
train <- data[data != test]
test <- Default[2001:4000,]
test.target <- default[2001:4000]

lr_vs.fit <- glm(default ~ income + balance,
                 family = binomial,
                 data = Default,
                 subset= train)

lr_vs.prob <- predict(lr_vs.fit, test, type="response")

lr_vs.pred <- rep("No", 2000)

lr_vs.pred[lr_vs.prob > .5] <- "Yes"

table(lr_vs.pred, test.target)

1- mean(lr_vs.pred == test.target) #2.6% error rate


#cv3
data <- 1:10000
test <- 4001:6000
train <- data[data != test]
test <- Default[4001:6000,]
test.target <- default[4001:6000]

lr_vs.fit <- glm(default ~ income + balance,
                 family = binomial,
                 data = Default,
                 subset= train)

lr_vs.prob <- predict(lr_vs.fit, test, type="response")

lr_vs.pred <- rep("No", 2000)

lr_vs.pred[lr_vs.prob > .5] <- "Yes"

table(lr_vs.pred, test.target)

1- mean(lr_vs.pred == test.target) #2.75% error rate

#cv4
data <- 1:10000
test <- 6001:8000
train <- data[data != test]
test <- Default[6001:8000,]
test.target <- default[6001:8000]

lr_vs.fit <- glm(default ~ income + balance,
                 family = binomial,
                 data = Default,
                 subset= train)

lr_vs.prob <- predict(lr_vs.fit, test, type="response")

lr_vs.pred <- rep("No", 2000)

lr_vs.pred[lr_vs.prob > .5] <- "Yes"

table(lr_vs.pred, test.target)

1- mean(lr_vs.pred == test.target) #2.65% error rate

#error rate is consistently 2-3%

#include student (dummy variable)
data <- 1:10000
test <- 1:2000
train <- data[data != test]
test <- Default[1:2000,]
test.target <- default[1:2000]

lr_vs.fit <- glm(default ~ income + balance + student,
                 family = binomial,
                 data = Default,
                 subset= train)

lr_vs.prob <- predict(lr_vs.fit, test, type="response")

lr_vs.pred <- rep("No", 2000)

lr_vs.pred[lr_vs.prob > .5] <- "Yes"

table(lr_vs.pred, test.target)

1- mean(lr_vs.pred == test.target) #2.65% error rate

summary(lr_vs.fit)

#adding student does not reduce error rate

set.seed(2)
attach(Default)

#compute estimates for the standard errors of the income and balance coef. using 
#(1) the bootstrap, (2) standard formula for computer std. error using glm() function

lr.fit <- glm(default ~ income + balance,
                       family = binomial,
                       data = Default)

summary(lr.fit)
#standard formula:
#income std. error = 4.985e-6
#balance std. error = 2.274e-4

boot.fn <- function(data, index){
  coef(glm(default ~ income + balance,
           family = binomial,
           data = data,
           subset = index))
}

library(boot)

boot(Default, boot.fn, R = 1000)
#bootstrap:
#income std. error = 4.227e-6
#balance std. error = 2.228e-4

#bootstrap is likely more accurate due to assumptions made by glm

#LOOCV using cv.glm() and for loop
set.seed(3)
attach(Weekly)

#logistic regression with all data
lr.fit <- glm(Direction ~ Lag1 + Lag2,
              family = binomial,
              data = Weekly)

#logistic regression with all but first observation
lr2.fit <- glm(Direction ~ Lag1 + Lag2,
               family = binomial,
               data = Weekly,
               subset = 2:length(Lag1))

Weekly[1,]

predict(lr2.fit,  Weekly)[1] #0.287534 < 0.5 therfore, "Down" which is correct

data <- 1:length(Lag1)
cv.pred <- rep(0,length(Lag1))

#for loop to perform LOOCV manually
for (i in 1:length(Lag1)){
  lr.fit <- glm(Direction ~ Lag1 + Lag2,
                family = binomial,
                data = Weekly,
                subset = data[data != i])
  cv.pred[i] <- predict(lr.fit,  Weekly)[i]
}

lr.prob <- rep("Down",length(Lag1))
lr.prob[cv.pred >0.5] <- "Up"

mean(Direction == lr.prob) #45.45% test error

#cross-validation on a simulated data set
set.seed(4)

#n = 100, p=1
x <- rnorm(100)
y <- x - 2*x^2 + rnorm(100)

#quadratic scatter plot
plot(x,y, col=2)

#array of size 10 filled with zeros
cv.LOOCV.error <- rep(0,4)

#create a for loop that iterates 1 through 4 (powers of x) that fits linear regression and calculates LOOCV
for (i in 1:4) {
  glm1.fit <- glm(y ~ poly(x,i), data = data.frame(cbind(x,y)))
  cv.LOOCV.error[i] <- cv.glm(data.frame(cbind(x,y)), glm1.fit)$delta[1]
}

cv.LOOCV.error #= 6.049406 1.026181 1.047449 1.061378 (average squared error)

set.seed(5)

#n = 100, p=1
x <- rnorm(100)
y <- x - 2*x^2 + rnorm(100)

cv.LOOCV.error <- rep(0,4)

for (i in 1:4) {
  glm2.fit <- glm(y ~ poly(x,i), data = data.frame(cbind(x,y)))
  cv.LOOCV.error[i] <- cv.glm(data.frame(cbind(x,y)), glm2.fit)$delta[1]
}

cv.LOOCV.error #= 7.642912 1.125060 1.109894 1.133182 (average squared error)

#the results with different seeds demonstrate that a higher order fit is better,
#but it's unclear which. A linear predictor performs the worst performs the worst for both

set.seed(4)

#n = 100, p=1
x <- rnorm(100)
y <- x - 2*x^2 + rnorm(100)

glm1.fit <- glm(y ~ poly(x,1), data = data.frame(cbind(x,y)))
summary(glm1.fit) #p-value for est. coef = 0.867
glm2.fit <- glm(y ~ poly(x,2), data = data.frame(cbind(x,y)))
summary(glm2.fit) #p-value for est. coefs = 0.688, <2e-16
glm3.fit <- glm(y ~ poly(x,3), data = data.frame(cbind(x,y)))
summary(glm3.fit) #p-value for est. coefs = 0.689, <2e-16, 0.685
glm4.fit <- glm(y ~ poly(x,4), data = data.frame(cbind(x,y)))
summary(glm4.fit) #p-value for est. coefs = 0.690, <2e-16, 0.686, 0.537

#the p-values for the estimated coefficients make sense because the p-value
#for the linear term is always non-significant and a model with only this term
#performed the worst. The performance of the model improved with the addition
#of the second term and this term's p-value is statistically significant.
#the other p-values for higher orders are not statistically significant
#and the model performance did not improve significantly with their inclusion

#Boston data set (problem 9)
attach(Boston)

mean(medv) #estimate of population mean = 22.53281
sd(medv)/sqrt(length(medv)) #estimate of standard error = 0.4088611

#bootstrap to find the standard error est. for the mean
boot.fn <- function(data, index){
  sd(data[index])/sqrt(length(medv))
}

boot(medv, boot.fn, R = 1000) #estimate of standard error = 0.4088611

#the two sd. error estimates are equal

#95% confidence interval for the estimated population mean
mean(medv) - 2*sd(medv)/sqrt(length(medv)) #= 21.71508
mean(medv) + 2*sd(medv)/sqrt(length(medv)) #= 23.35053

median(medv) #=21.2

#bootstrap to find the standard error est. for the median
boot.fn <- function(data, index){
  median(data[index])
}

boot(medv, boot.fn, R = 1000) #std. error = 0.3699221

#tenth percentile
quantile(medv, probs=.1) #=12.75

#bootstrap to find the standard error est. for the tenth percentile
boot.fn <- function(data, index){
  quantile(data[index], probs=.1)
}

boot(medv, boot.fn, R = 1000) #std. error = 0.4867335
