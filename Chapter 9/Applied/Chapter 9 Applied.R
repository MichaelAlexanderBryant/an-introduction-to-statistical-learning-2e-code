#simulated two-class data set with 100 observations and a visible, non-linear seperation
#between the two classes
x <- matrix(rnorm(100*2), ncol=2)
y <- c(rep(-1,50), rep(1,50))
x[y == 1, ] <- x[y == 1, ] + 4.5
plot(x, col = (y + 5)/2, pch = 19)
dat <- data.frame(x = x, y = as.factor(y))

train <- sample(100, 80)
y.train <- y[train]
y.test <- y[-train]

#show that a SVM with a polynomial/radial kernel outperforms a SVC on the training data
library(e1071)
svmfit.poly <- svm(y ~ ., data = dat, subset=train, kernel = "polynomial",
              degree = 10, cost = 2, scale = FALSE) #poly fit
plot(svmfit.poly, dat[train, ])
svmfit.train.pred <- predict(svmfit.poly, newdata=dat[train,])
table(svmfit.train.pred, y.train)
(38+42)/(38+42) #100% train accuracy

svmfit.test.pred <- predict(svmfit.poly, newdata=dat[-train,])
table(svmfit.test.pred, y.test)
(12+7)/(12+7+1) #95% test accuracy

svmfit.lin <- svm(y ~ ., data = dat, subset=train, kernel = "linear",
                   cost = 2, scale = FALSE) #linear fit
plot(svmfit.lin, dat[train, ])
svmfit.train.pred <- predict(svmfit.lin, newdata=dat[train,])
table(svmfit.train.pred, y.train)
(38+42)/(38+42) #100% train accuracy

svmfit.test.pred <- predict(svmfit.lin, newdata=dat[-train,])
table(svmfit.test.pred, y.test)
(12+8)/(12+8) #100% test accuracy

#a linear kernel does not overfit the training data and therefore has a higher
#test accuracy

#SVM can be used to create non-linear decision boundaries, show that logistic
#regression can be used to obtain non-linear decision boundaries

#simulated data set with n=500 and p=2 with quadratic decision boundary
x1 <- runif(500) - 0.5
x2 <- runif(500) - 0.5
y <- 1*(x1^2 - x2^2 > 0)
plot(x1, x2, col = (y + 5), pch = 19)

#quadratic decision boundary using logistic regression
log.fit <- glm(y~poly(x1,2)+poly(x2,2), family=binomial)
log.prob <- predict(log.fit, data.frame(x1,x2), type="response")
log.pred <- rep(0,500)
log.pred[log.prob > .5] <- 1
plot(x1,x2, col=log.pred+1, pch=19)

dat <- data.frame(x1 = x1, x2 = x2, y = as.factor(y))

#svc
svc.fit <- svm(y~., data=dat, kernel="linear", cost=.1, scale=FALSE)
svc.pred <- predict(svc.fit, newdata=dat)
plot(x1,x2, col=as.integer(svc.pred)+ 2, pch=19)
summary(svc.pred) #all points categorized as same class

#svm with polynomial kernel
svm.fit <- svm(y~., data=dat, kernel="polynomial", degree=10, cost=.1, scale=FALSE)
svm.pred <- predict(svm.fit, newdata=dat)
plot(x1,x2, col=as.integer(svm.pred)+ 2, pch=19)
summary(svm.pred) #all points categorized as same class

#svm with radial kernel
svm.fit <- svm(y~., data=dat, kernel="radial", gamma=5, cost=.1, scale=FALSE)
svm.pred <- predict(svm.fit, newdata=dat)
plot(x1,x2, col=as.integer(svm.pred)+ 2, pch=19)
summary(svm.pred) #true decision boundary found

#logistic is able to find a decision boundary that a SVC and polynomial kernel
#SVM cannot. given logistic is easier to interpret, it's worth trying logistic
#prior to using something like a SVC/SVM

#investigate the claim that a SVC with a small cost that misclassifies a couple
# training observations may perform better on test data than one with a huge
#cost that does not misclassify any training observations

#simulate two-class data with p=2 such that classes are just barely linearly seperable
set.seed(1)
x1 <- runif(1000) - 0.5
x2 <- runif(1000) - 0.5
y <- 1*(x1 - x2 > 0)
plot(x1, x2, col = (y + 5), pch = 19)
dat <- data.frame(x1 = x1, x2 = x2, y = as.factor(y))

train <- sample(1000, 800)
y.train <- y[train]
y.test <- y[-train]

#cross-validation error rates for SVC on train data with a range of cost values
tune.out <- tune(method = svm, y ~ ., data = dat[train,],
                 kernel = "linear",
                 ranges = list(
                   cost = as.character(seq(1,50,by=.5))))

cost_list.cv <- as.numeric(summary(tune.out)$performances[,'cost'])
error_list.cv <- as.numeric(summary(tune.out)$performances[,'error'])


plot(x =cost_list.cv*.5, y=error_list.cv)
lines(x =cost_list.cv*.5, y=error_list.cv)

#compute test error for same range of cost values
cost_list <- as.character(seq(1,50,by=.5))
test_error <- rep(NA, length(cost_list))

for (i in 1:length(cost_list)){
  svc.fit <- svm(y~., data=dat, subset=train, kernel="linear", cost=cost_list[i],
                 scale=FALSE)
  svc.pred <- predict(svc.fit, newdata=dat[-train,])
  accuracy <- rep(0,length(svc.pred))
  accuracy[svc.pred == y.test] <- 1
  test_error[i] <- 1 - sum(accuracy)/length(accuracy)
  
}

plot(x=cost_list, y=test_error)
lines(x=cost_list, y=test_error)

#based on the training CV, a cost value of about 3 onward seems like it would suffice
#however, the test error for a cost up to 20 is 2% higher than the train CV error
#this demonstrates that choosing a higher value for cost may be better

#use support vector approach to predict mpg (high/low) using Auto data set
library(ISLR2)
high_mpg <- rep(0, length(Auto$mpg))
high_mpg[Auto$mpg > median(Auto$mpg)] <- 1
View(Auto)
Auto_mod <- data.frame(Auto, y=high_mpg)
Auto_mod <- subset(Auto_mod, select=-c(mpg))

#svc CV with various cost values
tune.out <- tune(method = svm, y ~ ., data = Auto_mod,
                 kernel = "linear",
                 ranges = list(
                   cost = as.character(seq(1,25,by=1))))

#plot CV error against cost value
par(mfrow=c(1,1))
cost_list.cv <- as.numeric(summary(tune.out)$performances[,'cost'])
error_list.cv <- as.numeric(summary(tune.out)$performances[,'error'])
plot(x =cost_list.cv, y=error_list.cv)
lines(x =cost_list.cv, y=error_list.cv)

#svm (polynomial kernel) CV with various cost and degree values
tune.out <- tune(method = svm, y ~ ., data = Auto_mod,
                 kernel = "polynomial",
                 ranges = list(
                   degree = as.character(seq(2,5, by=1)),
                   cost = as.character(seq(1,25,by=1))))

#plot CV error against cost value for degrees (2 to 5)
par(mfrow=c(2,2))
for (i in 2:5){
  cost_list.cv <- as.numeric(summary(tune.out)$performances[summary(tune.out)$performances[ , 'degree']==i,'cost'])
  error_list.cv <- as.numeric(summary(tune.out)$performances[summary(tune.out)$performances[ , 'degree']==i,'error'])
  plot(x =cost_list.cv, y=error_list.cv)
  lines(x =cost_list.cv, y=error_list.cv)
  
}

#svm (radial kernel) CV with various cost and gamma values
tune.out <- tune(method = svm, y ~ ., data = Auto_mod,
                 kernel = "radial",
                 ranges = list(
                   gamma = c(.1,1,5,10),
                   cost = as.character(seq(1,25,by=1))))

#plot CV error against cost value for gammas (.1,1,5,10)
par(mfrow=c(2,2))
for (i in c(.1,1,5,10)){
  cost_list.cv <- as.numeric(summary(tune.out)$performances[summary(tune.out)$performances[ , 'gamma']==i,'cost'])
  error_list.cv <- as.numeric(summary(tune.out)$performances[summary(tune.out)$performances[ , 'gamma']==i,'error'])
  plot(x =cost_list.cv, y=error_list.cv)
  lines(x =cost_list.cv, y=error_list.cv)
  
}

#radial with a gamma value of 0.1 and a cost value of 3 to 5 performed the best

#OJ data set
set.seed(1)
train <- sample(1:nrow(OJ), 800)
purchase.train <- OJ$Purchase[train]
purchase.test <- OJ$Purchase[-train]

#SVC with cost = 0.01
svc.fit <- svm(Purchase ~ ., data = OJ, subset=train, kernel = 'linear', cost = 0.01)
summary(svc.fit) #435 support vectors

#training error rate
svc.train.pred <- predict(svc.fit, OJ[train, ])
table(svc.train.pred, purchase.train)
1 - (420+240)/(420+240+65+75) #17.5% error rate

#test error rate
svc.test.pred <- predict(svc.fit, OJ[-train, ])
table(svc.test.pred, purchase.test)
1 - (153+69)/(153+69+15+33) #17.8% error rate

#use tune for cost values in range of 0.01 to 10
set.seed(1)
tune.out <- tune(method = svm, Purchase ~ ., data = OJ[train,],
                 kernel = "linear",
                 ranges = list(
                   cost = c(0.01, 0.1, 1, 2.5, 5, 7.5, 10)))

summary(tune.out) #cost=2.5 is best


#re-fit with cost = 2.5
svc.fit <- svm(Purchase ~ ., data = OJ, subset=train, kernel = 'linear', cost = 2.5)
summary(svc.fit) #329 support vectors

#training error rate
svc.train.pred <- predict(svc.fit, OJ[train, ])
table(svc.train.pred, purchase.train)
1 - (422+245)/(422+245+63+70) #16.6% error rate

#test error rate
svc.test.pred <- predict(svc.fit, OJ[-train, ])
table(svc.test.pred, purchase.test)
1 - (156+74)/(156+74+12+28) #14.8% error rate


#repeat with radial kernel and default gamma

svm.fit <- svm(Purchase ~ ., data = OJ, subset=train, kernel = 'radial', cost = 0.01)
summary(svm.fit) #634 support vectors

#training error rate
svm.train.pred <- predict(svm.fit, OJ[train, ])
table(svm.train.pred, purchase.train)
1 - (485)/(485+315) #39.4% error rate

#test error rate
svm.test.pred <- predict(svm.fit, OJ[-train, ])
table(svm.test.pred, purchase.test)
1 - (168)/(168+102) #37.8% error rate

#use tune for cost values in range of 0.01 to 10
set.seed(1)
tune.out <- tune(method = svm, Purchase ~ ., data = OJ[train,],
                 kernel = "radial",
                 ranges = list(
                   cost = c(0.01, 0.1, 1, 2.5, 5, 7.5, 10)))

summary(tune.out) #cost=1 is best


#re-fit with cost = 1
svm.fit <- svm(Purchase ~ ., data = OJ, subset=train, kernel = 'radial', cost = 1)
summary(svm.fit) #373 support vectors

#training error rate
svm.train.pred <- predict(svm.fit, OJ[train, ])
table(svm.train.pred, purchase.train)
1 - (441+238)/(441+238+44+77) #15.1% error rate

#test error rate
svm.test.pred <- predict(svm.fit, OJ[-train, ])
table(svm.test.pred, purchase.test)
1 - (151+69)/(151+69+17+33) #18.5% error rate

#repeat with polynomial kernel and degree = 2

svm.fit <- svm(Purchase ~ ., data = OJ, subset=train, kernel = 'polynomial', degree=2, cost = 0.01)
summary(svm.fit) #636 support vectors

#training error rate
svm.train.pred <- predict(svm.fit, OJ[train, ])
table(svm.train.pred, purchase.train)
1 - (484+18)/(484+18+297+1) #37.3% error rate

#test error rate
svm.test.pred <- predict(svm.fit, OJ[-train, ])
table(svm.test.pred, purchase.test)
1 - (167+4)/(167+4+98+1) #36.7% error rate

#use tune for cost values in range of 0.01 to 10
set.seed(1)
tune.out <- tune(method = svm, Purchase ~ ., data = OJ[train,],
                 kernel = "polynomial",
                 degree = 2,
                 ranges = list(
                   cost = c(0.01, 0.1, 1, 2.5, 5, 7.5, 10)))

summary(tune.out) #cost=2.5 is best


#re-fit with cost = 2.5
svm.fit <- svm(Purchase ~ ., data = OJ, subset=train, kernel = 'polynomial',degree=2, cost = 2.5)
summary(svm.fit) #394 support vectors

#training error rate
svm.train.pred <- predict(svm.fit, OJ[train, ])
table(svm.train.pred, purchase.train)
1 - (452+222)/(452+222+93+33) #15.8% error rate

#test error rate
svm.test.pred <- predict(svm.fit, OJ[-train, ])
table(svm.test.pred, purchase.test)
1 - (154+60)/(154+60+42+14) #20.7% error rate

#SVC seems to give the best results