##4.7.1 The Stock Market Data

#set the working directory
getwd()
setwd("C:/Users/malex/Documents/Introduction to Statistical Learning Labs/Chapter 4/Lab")

#read and view data set
s_market <- read.csv("Smarket.csv", stringsAsFactors = TRUE)
View(s_market)

#check for na values
which(is.na(s_market))

#variable names, dimension, and summary of data set
names(s_market)
dim(s_market)
summary(s_market)

#scatter matrix
pairs(s_market, col=2)

#correlations excluding direction (qualitative variable)
cor(s_market[ ,-9])

#volume and year has the only significant correlation at 0.54
#volume = number of shares traded on the previous day (in billions)
#therefore, number of shares traded on the previous day is increasing over time (2001 to 2005)
attach(s_market)
plot(Volume)

##4.7.2 Logistic Regression

#logistic regression fit (glm is generalized linear model)
glm.fits <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
                data = s_market,
                family = binomial)
summary(glm.fits)

#all p-values are >0.05

#different ways of accessing coefficients
coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[,4]

#make predictions of the response (type can be set to indicate logit, etc.)
#with no data set provided, the training set is used
glm.probs <- predict(glm.fits, type = "response")
#first 10 predictions
glm.probs[1:10]
#where the dummy variable was created such that the probabilities are for an "Up" response
contrasts(Direction)

#create a vector of 1250 "Down" elements
glm.pred <- rep("Down", 1250)
#replace "Down" with "Up" if probabilities are greater than .5
glm.pred[glm.probs > .5] = "Up"

#confusion matrix
table(glm.pred, Direction)

#accuracy
(507+145)/1250
#alternative to above
mean(glm.pred == Direction)

#the training set accuracy is slightly better than just guessing
summary(Direction)
648/(648+602)

#however, training set accuracy may overfit, so a hold out set is necessary
#to estimate real world performance

#save 2005 as a holdout set
#train is a boolean vector
train <- (Year < 2005)
#negation of a boolean vector converts True to False and False to True
#save the 2005 data
s_market.2005 <- s_market[!train,]
dim(s_market.2005)
#vector gets saved to values, matrices/dataframes to data
Direction.2005 <- Direction[!train]

#fit logistic regression model to training data for only Lag1 and Lag2
glm.fits <- glm(Direction ~ Lag1 + Lag2,
                data = s_market,
                family = binomial,
                subset = train)

#predict on hold out set
glm.probs <- predict(glm.fits,
                     s_market.2005,
                     type = "response")

#create a vector the size of the hold out set with "Down" elements
glm.pred <- rep("Down", 252)
#replace "Down" with "Up" where probabilities are greater than .5
glm.pred[glm.probs > .5] <- "Up"
#confusion matrix
table(glm.pred, Direction.2005)
#accuracy
mean(glm.pred == Direction.2005)
#error rate
mean(glm.pred != Direction.2005)
#58% of "Up" predictions are correct (106/(76+106))
#could be from random chance

#predict for specific values of Lag1 and Lag2
predict(glm.fits,
        newdata = data.frame(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -0.8)),
        type = "response")

##4.73 Linear Discriminant Analysis

#lda is part of MASS library
library(MASS)

#fit lda
lda.fit <- lda(Direction ~ Lag1 + Lag2,
               data = s_market,
               subset = train)

#prior probabilities (pi_sub_k_hat)
#group means (mu_sub_k estimates)
#suggest previous two day returns are negative when the market increases
#coefficients: -0.642*Lag1 - 0.514*Lag2 predicts increase when large and decrease otherwise
lda.fit

#plot of discriminants using previous equation for training observations
plot(lda.fit)

#predict contains 1. class, 2. posterior probability that corresponding observation belongs to kth class,
#3. linear discriminants
lda.pred <- predict(lda.fit, s_market.2005)
names(lda.pred)

#almost identical to logistic regression
lda.class <- lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)

#posterior threshold of 50% allows the recreation of predictions contained in lda.pred$class
sum(lda.pred$posterior[,1] >= .5)
sum(lda.pred$posterior[,1] < .5)

#posterior probability output corresponds to a market decrease
lda.pred$posterior[1:20,1]
lda.class[1:20]

#use 90% threshold if we want to predict a market decrease with higher certainty
#but we can't be that certain on any day since the highest is 52.02%
sum(lda.pred$posterior[,1] > .9) #=0
max(lda.pred$posterior[,1]) #=0.5202

##4.7.4 Quadratic Discriminant Analysis

#qda is a part of the MASS library
qda.fit <- qda(Direction ~ Lag1 + Lag2,
               data = s_market,
               subset = train)

qda.fit

qda.class <- predict(qda.fit, s_market.2005)$class
table(qda.class, Direction.2005)
#qda seems to perform better than lda and logistic regression
#use larger data set to confirm
mean(qda.class == Direction.2005) #=0.60

##4.7.5 Naive Bayes

#import library with naive Bayes
library(e1071)

#models each quantitative feature using Gaussian distribution, by default
nb.fit <- naiveBayes(Direction ~ Lag1 + Lag2,
                     data = s_market,
                     subset = train)
#for conditional probabilities [,1] is the mean, [,2] is the std
nb.fit
mean(Lag1[train][Direction[train] == "Down"])
sd(Lag1[train][Direction[train] == "Down"])

nb.class <- predict(nb.fit, s_market.2005)
table(nb.class, Direction.2005)
#naive Bayes is better than LDA and logistic regression, worse than QDA
mean(nb.class == Direction.2005) #=59%

#estimates of probabilities
nb.preds <- predict(nb.fit, s_market.2005, type = 'raw')
nb.preds[1:5,]

##4.7.6 K-Nearest Neighbors

#import library with knn
library(class)

#create training and test matricies and train-target array
#cbind = column bind
train.X <- cbind(Lag1,Lag2)[train,]
test.X <- cbind(Lag1,Lag2)[!train,]
train.Direction <- Direction[train]

set.seed(1)

#use k = 1
knn.pred <- knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2005)
#accuracy
(83+43)/252 #=50.0%

#use k = 3
knn.pred <- knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)
(48+87)/252 #=53.6%, alternatively mean(knn.pred == Direction.2005)

#QDA performed the best on the stock market data

#moving onto Caravan data set
caravan <- read.csv("Caravan.csv", stringsAsFactors = TRUE)
attach(caravan)

#only 348 purchased insurance, 5474 did not (6%)
summary(Purchase)

#knn uses distance for predictions, therefore need to standardize (excluding Purchase)
standardized.X <- scale(caravan[,-86])
#variance comparison
var(caravan[,1])
var(caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])
#standardize: sd=1, mean=0

#train-test split
#first 1000 test, rest train
test <- 1:1000
train.X <- standardized.X[-test,]
test.X <-standardized.X[test,]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k = 1)
mean(test.Y != knn.pred) #=11.8% error rate
mean(test.Y != "No") #5.9% of test is "Yes, so guessing "No" on all would yield a 94.1% accuracy (5.9% error rate)
summary(test.Y)
59/(941+59)

#if the company is only interested in targeting individuals who are likely to buy,
#then error rate/accuracy does not matter, instead what matters is sensitivity (TP rate)
table(knn.pred, test.Y)
9/(9+68) #=11.7% true positive rate (TP/(TP+FN))

knn.pred <- knn(train.X, test.X, train.Y, k = 3)
table(knn.pred, test.Y)
5/(21+5) #=19.2% true positive rate

knn.pred <- knn(train.X, test.X, train.Y, k = 5)
table(knn.pred, test.Y)
4/(11+4) #=26.7% true positive rate, over four times higher than guessing (5.9% guessing)

#comparison to logistic regression
glm.fits <- glm(Purchase ~., data = caravan, family = binomial, subset = -test)
glm.probs <- predict(glm.fits, caravan[test,], type = "response")
glm.pred <- rep("No", 1000)
glm.pred[glm.probs >.5] <- "Yes"
table(glm.pred, test.Y)
0/(0+7) #=0%

#changing the probability threshold to 0.25
glm.pred <- rep("No", 1000)
glm.pred[glm.probs >.25] <- "Yes"
table(glm.pred, test.Y)
11/(22+11) #=33.3%, five times better than random guessing

##4.7.7 Poisson Regression

library(ISLR2)
dim(Bikeshare)
names(Bikeshare)

#least squares
mod.lm <- lm(bikers ~ mnth + hr + workingday + temp + weathersit,
             data = Bikeshare)
summary(mod.lm)
#example interpretation: feb coef = 6.845 meaning all else held constant,
#there are 7 more bikers in feb
#jan and hr0 are missing due to dropping the first to avoid multicollinearity

#remove last instead of first
contrasts(Bikeshare$hr) = contr.sum(24)
contrasts(Bikeshare$mnth) = contr.sum(12)

#fit "drop last" data
mod.lm2 <- lm(bikers ~ mnth + hr + workingday + temp + weathersit,
              data = Bikeshare)

#using this approach, the estimate of the last levels is equal to the negative
#of the sum of the coefficient estimates for all of the other levels
#coef for month and hour now mean that (example) holding all variables constant
#Jan coef = -46.087 meaning there are 46 less people than yearly average in Jan
summary(mod.lm2)

sum((predict(mod.lm) - predict(mod.lm2))^2) #~=0, no difference in codings
#alternative to above
all.equal(predict(mod.lm), predict(mod.lm2)) #=TRUE

#coef of first 11 months and using neg sum to compute last month
coef.months <- c(coef(mod.lm2)[2:12], -sum(coef(mod.lm2)[2:12]))

#plot coefficients vs months
plot(coef.months, xlab="Month", ylab="Coefficient", xaxt = "n", col="blue",
     pch = 19, type = "o")
axis(side = 1, at = 1:12, labels = c("J", "F", "M", "A", "M", "J", "J", "A",
                                   "S", "O", "N", "D"))

#coef of hr
coef.hours <- c(coef(mod.lm2)[13:35], -sum(coef(mod.lm2)[13:35]))

#plot coefficients vs hours
plot(coef.hours, xlab = "Hour", ylab = "Coefficient", col = "blue", pch = 19,
     type = "o")


#poisson regression
mod.pois <- glm(bikers ~ mnth + hr + workingday + temp + weathersit,
                data = Bikeshare,
                famil = poisson)
summary(mod.pois)

#coef of first 11 months and using neg sum to compute last month
coef.months <- c(coef(mod.pois)[2:12], -sum(coef(mod.pois)[2:12]))

#plot coefficients vs months
plot(coef.months, xlab="Month", ylab="Coefficient", xaxt = "n", col="blue",
     pch = 19, type = "o")
axis(side = 1, at = 1:12, labels = c("J", "F", "M", "A", "M", "J", "J", "A",
                                     "S", "O", "N", "D"))

#coef of hr
coef.hours <- c(coef(mod.pois)[13:35], -sum(coef(mod.pois)[13:35]))

#plot coefficients vs hours
plot(coef.hours, xlab = "Hour", ylab = "Coefficient", col = "blue", pch = 19,
     type = "o")

#plot comparison of predictions between lm2 and pois
#must use "response" to get pred in exp(b0 + b1*X1 + ...) instead of b0 + b1*X1+...
#which is default
plot(predict(mod.lm2), predict(mod.pois, type="response"))
abline(0,1,col=2,lwd=3)

#predictions are correlated between the two models
#however, poisson tends to be larger for very low or very high ridership
#because linear model predicts negative ridership