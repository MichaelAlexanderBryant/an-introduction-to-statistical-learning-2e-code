###9.6 Lab: Support Vector Machines

##9.61 Support Vector Classifier

set.seed(1)
#simulated data
x <- matrix(rnorm(20*2), ncol=2)
y <- c(rep(-1,10), rep(1,10))
x[y == 1, ] <- x[y == 1, ] + 1
plot(x, col = (3-y)) #not linearly seperable

dat <- data.frame(x = x, y = as.factor(y)) #svm requires y be a factor
library(e1071)
#when cost is small, the margins are wide, and vice versa
#linear kernel = support vector classifier
#scale = False means no standardization
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)
plot(svmfit, dat) #support vectors are plotted as crosses
svmfit$index #support vector indices
summary(svmfit)

#repeat with a smaller cost parameter
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = .1, scale = FALSE)
plot(svmfit, dat) #support vectors are plotted as crosses
svmfit$index #support vector indices
summary(svmfit) #more support vectors because the margins are wider

#built in cross-validation function (default: 10-fold) tune()
#pass list of cost values to evaluate
set.seed(1)
tune.out <- tune(svm, y ~ ., data = dat, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out) #best parameter is cost = .1
bestmod <- tune.out$best.model
summary(bestmod) #best parameters are stored and accessed using $best.model

#generate test set
xtest <- matrix(rnorm(20*2), ncol =2)
ytest <- sample(c(-1,1), 20, rep = TRUE)
xtest[ytest == 1, ] <- xtest[ytest == 1, ] + 1
testdat <- data.frame(x = xtest, y = as.factor(ytest))

ypred <- predict(bestmod, testdat)
table(predict = ypred, truth = testdat$y) 
(9+8)/(9+8+1+2) #=85% accuracy

#using a less optimal cost value
svmfit <- svm(y ~ ., data = dat, kernel = "linear",
              cost = 0.01, scale = FALSE)
ypred <- predict(svmfit, testdat)
table(predict = ypred, truth = testdat$y)
(11+3)/(11+3+6) #70% accuracy

#create linearly separable data
x[y == 1, ] <- x[y == 1, ] + 0.5
plot(x, col = (y + 5)/2, pch = 19)

dat <- data.frame(x = x, y = as.factor(y))
#use large cost function for a narrow margin
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 1e5)
summary(svmfit) #3 support vectors
plot(svmfit, dat)

#try a smaller cost function to increase the margin
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 1)
summary(svmfit)
plot(svmfit, dat) #7 support vectors

##9.6.2 Support Vector Machine

#to create an SVM change kernel to either polynomial or radial
#polynomial uses the degree option for hyperparameter tuning
#radial uses the gamma option for hyperparameter tuning

#simulate data with a non-linear class boundary
set.seed(1)
x <- matrix(rnorm(200*2), ncol = 2)
x[1:100, ] <- x[1:100, ] + 2
x[101:150, ] <- x[101:150, ] - 2
y <- c(rep(1,150), rep(2,50))
dat <- data.frame(x=x, y=as.factor(y))
plot(x, col=y)

#split data, fit, and plot
train <- sample(200, 100)
svmfit <- svm(y ~ ., data = dat[train, ], kernel = "radial",
              gamma = 1, cost = 1)
plot(svmfit, dat[train, ]) #there are a fair number of training errors

summary(svmfit) #31 support vectors

#increasing the cost would create a more irregular decision boundary
#which would decrease the training errors, but may overfit the data
svmfit <- svm(y ~ ., data = dat[train, ], kernel = "radial",
           gamma = 1, cost = 1e5)
plot(svmfit, dat[train, ])

rm(svm) #added svm as a dataframe which created an error while tuning (below)

#use cross-validation to find the best gamma and cost
set.seed(1)
tune.out <- tune(method = svm, y ~ ., data = dat[train,],
                 kernel = "radial",
                 ranges = list(
                   cost = c(0.1, 1, 10, 100, 1000),
                   gamma = c(0.5, 1, 2, 3, 4)))

summary(tune.out) #best: 1 cost, .5 gamma

svm.pred <- predict(tune.out$best.model, newdata = dat[-train,])
table(true = dat[-train, "y"], pred = svm.pred)
(67+21)/(67+21+2+10) #88% accuracy

##9.6.3 ROC Curves
install.packages('ROCR')
library(ROCR)

#create a function to plot ROC curve
rocplot <- function(pred, truth, ...){
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf,...)
}

#use decision.values = TRUE to output the fitted values
svmfit.opt <- svm(y ~ ., data = dat[train, ],
                  kernel = "radial", gamma = 2, cost = 1,
                  decision.values = T)
#fitted values determine which side of the decision boundary that a vector is on
#if the vector is negative then b0+b1*x1+b2x2+...<0, if positive >0
fitted <- attributes(predict(svmfit.opt, dat[train, ], decision.values = T))$decision.values

#plot roc curve
par(mfrow = c(1,2))
#negative of fitted values correspond to class 1, positive values to class 2
rocplot(-fitted, dat[train, "y"], main = "Training Data")

#increasing gamma produces a more flexible fit and improve accuracy
svmfit.flex <- svm(y ~ ., data = dat[train, ],
                   kernel = "radial", gamma = 50, cost = 1,
                   decision.values = T)
fitted <- attributes(predict(svmfit.flex, dat[train, ], decision.values = T))$decision.values
rocplot(-fitted, dat[train, "y"], add = T, col = "red")

#now look at the performance on test data (gamma=2 has best performance)
fitted <- attributes(predict(svmfit.opt, dat[-train, ], decision.values = T))$decision.values
rocplot(-fitted, dat[-train, "y"], main = "Test Data")
fitted <- attributes(predict(svmfit.flex, dat[-train, ], decision.values = T))$decision.values
rocplot(-fitted, dat[-train, "y"], add = T, col = "red")

##9.6.4 SVM with Multiple Classes

#if more than two classes then the svm function will perform one-versus-one
set.seed(1)
x <- rbind(x, matrix(rnorm(50*2), ncol = 2))
y <- c(y, rep(0, 50))
x[y==0, 2] <- x[y == 0, 2] + 2
dat <- data.frame(x=x, y = as.factor(y))
par(mfrow = c(1,1))
plot(x, col=(y+1))

#fit and plot
svmfit <- svm(y ~ ., data = dat, kernel = "radial",
              cost = 10, gamma = 1)
plot(svmfit, dat)

#e1071 can be used to perform support vector regression (SVR)
#svm() automatically handles a continuous target with SVR

##9.6.5 Application to Gene Expression Data

#use Khan data set which is already split into training and test data
library(ISLR2)
names(Khan)

#2308 genes, 63 training observations, 20 test observations
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)
table(Khan$ytrain) #multiclass (4 classes)
table(Khan$ytest)

#very large number of features relative to observations
#therefore use linear kernel, because additional flexibility is unnecessary
dat <- data.frame(x = Khan$xtrain, y = as.factor(Khan$ytrain))
out <- svm(y ~ ., data = dat, kernel = "linear", cost = 10)
summary(out)
table(out$fitted, dat$y) #100% training accuracy

#test set
dat.te <- data.frame(
  x = Khan$xtest,
  y = as.factor(Khan$ytest))
pred.te <- predict(out, newdata = dat.te)
table(pred.te, dat.te$y)
(3+6+4+5)/(3+6+4+5+2) #90% test accuracy