#Conceptual problem: plotting classification error, gini index, and entropy for a two-class problem
#as a function of the probability of one class
prob1 <- as.character(seq(0,1,by=0.01))
options(digits=5)
prob1<- as.double(prob1)
prob0 <- 1 - prob1

ce <- 1 - max(prob0, prob1)
gi <- prob0*(1-prob0) + prob1*(1-prob1)
ent <- -prob0*log(prob0) - prob1*log(prob1)

for (i in 1:length(prob0)){
  ce[i] <- 1 - max(prob0[i], prob1[i])
}
matplot(prob1, cbind(ce,gi,ent))

##Applied
#use randrom forest on Boston data set using mtry = 6, and ntree = 25 and 200.
#create a plot displaying the test error resulting from different ranges of mtry and ntree

library(ISLR2)
attach(Boston)
sum(is.na(Boston))

names(Boston)

library(randomForest)
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
boston.test <- Boston[-train, "medv"]

library(randomForest)

#mtry=6, ntree=25
rf.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 6, ntree=25, importance = T)
yhat.rf <- predict(rf.boston, newdata = Boston[-train,])
mean((yhat.rf - boston.test)^2) #=18.784 MSE
#mtry=6, ntree=200
rf.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 6, ntree=200, importance = T)
yhat.rf <- predict(rf.boston, newdata = Boston[-train,])
mean((yhat.rf - boston.test)^2) #=20.191 MSE

mse <- rep(NA, length(names(Boston))-1)

for (i in 2:length(names(Boston))-1){
  rf.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = i, importance = T)
  yhat.rf <- predict(rf.boston, newdata = Boston[-train,])
  mse[i] <- mean((yhat.rf - boston.test)^2)
}
variables <- 2:length(names(Boston))-1
plot(x=variables, y = mse)
title("Varying mtry")
lines(x=variables, y = mse)


mse <- rep(NA, 500)

for (i in 1:500){
  rf.boston <- randomForest(medv ~ ., data = Boston, subset = train, ntrees = i, importance = T)
  yhat.rf <- predict(rf.boston, newdata = Boston[-train,])
  mse[i] <- mean((yhat.rf - boston.test)^2)
}
trees <- 1:500
plot(x=trees, y = mse)
title("Varying ntrees")
lines(x=trees, y=mse)

#apply regression tree to predict Sales in Carseats data set
attach(Carseats)

set.seed(2)
train <- sample(1:nrow(Carseats), nrow(Carseats)/2)
y.test <- Carseats[-train, "Sales"]

library(tree)

rt.fit <- tree(Sales ~ ., data = Carseats, subset = train)
summary(rt.fit)
plot(rt.fit)
text(rt.fit, pretty = 0)

rt.pred <- predict(rt.fit, newdata = Carseats[-train,])
mean((rt.pred - y.test)^2) #=4.47 MSE

#use CV to determine optimal tree complexity
cv.carseats <- cv.tree(rt.fit)
plot(cv.carseats$size, cv.carseats$dev, type = "b")
#14 leaves is already optimal

#use bagging
bagging.carseats <- randomForest(Sales ~ ., data = Carseats, subset = train, mtry = length(names(Carseats))-1, importance = T)
yhat.bagging <- predict(bagging.carseats, newdata = Carseats[-train,])
mean((yhat.bagging - y.test)^2) #=2.5546 MSE (better than a regression tree)

#feature importance (price, shelveloc, and compprice are most important)
importance(bagging.carseats)
varImpPlot(bagging.carseats)

mse <- rep(NA,length(names(Carseats))-1)

for (i in 2:length(names(Carseats))-1){
  rf.carseats <- randomForest(Sales ~ ., data = Carseats, subset = train, mtry = i , importance = T)
  yhat.rf <- predict(rf.carseats, newdata = Carseats[-train,])
  mse[i] <- mean((yhat.rf - y.test)^2) #=2.5546 MSE (better than a regression tree)
}

mtry = 2:length(names(Carseats))-1
plot(x=mtry, y=mse)
which.min(mse)

mse[10] #=2.5571 MSE (bagging), random forest increases MSE

#use BART

library(BART)

x <- Carseats[, names(Carseats) != "Sales"]
y <- Carseats[, "Sales"]
xtrain <- x[train,]
ytrain <- y[train]
xtest <- x[-train,]
ytest <- y[-train]

set.seed(3)
bartfit <- gbart(xtrain, ytrain, x.test = xtest)

yhat.bart <- bartfit$yhat.test.mean
mean((y.test - yhat.bart)^2) #=1.4347 MSE, best MSE

#switch to OJ data set (problem 9)
attach(OJ)

View(OJ)
sum(is.na(OJ))

set.seed(5)
train <- sample(1:nrow(OJ), 800)
oj.test <- OJ[-train, "Purchase"]

oj.tree <- tree(Purchase ~ ., data = OJ, subset = train)
summary(oj.tree) #error rate = 17%, terminal nodes = 9

oj.tree
plot(oj.tree)
text(oj.tree, pretty = 0)

oj.pred <- predict(oj.tree, OJ[-train, ], type = "class")
table(oj.pred, oj.test)
1 - ((136+83)/(136+83+33+18)) #=18.89% error rate

cv.oj <- cv.tree(oj.tree)
plot(cv.oj$size, cv.oj$dev, type = "b") #size = 5 optimal

prune.oj <- prune.tree(oj.tree, best = 5)
summary(prune.oj) #=19.5% error rate (unpruned had lower train error rate)

oj.pred <- predict(prune.oj, OJ[-train, ], type = "class")
table(oj.pred, oj.test)
1 - (133+89)/(133+89+21+27) #=17.78% error rate (pruned tree has lower test error rate)

#use boosting to predict Salary in Hitters data set
attach(Hitters)
sum(is.na(Hitters$Salary))
Hitters <- na.omit(Hitters)

Hitters$Salary <- log(Hitters$Salary)

train <- sample(1:nrow(Hitters), 200)
salary.test <- Hitters[-train,"Salary"]

library(gbm)
set.seed(7)

train_mse <- rep(NA, 101)
test_mse <- rep(NA, 101)

for (i in 0:100){
  boost.hitters <- gbm(Salary ~ ., data = Hitters[train,], distribution = "gaussian", n.trees = 1000,
                      shrinkage = i*.01)
  hitters.train.pred <- predict(boost.hitters, newdata = Hitters[train, ], n.trees = 1000)
  hitters.pred <- predict(boost.hitters, newdata = Hitters[-train, ], n.trees = 1000)
  train_mse[i] <- mean((Hitters[train,"Salary"] -  hitters.train.pred)^2)
  test_mse[i] <- mean((salary.test - hitters.pred)^2)
}

shrinkage = .01*(0:100)
matplot(x = shrinkage, y = cbind(train_mse, test_mse))
which.min(test_mse) #=6
test_mse[6] #=0.24462

lm.fit <- lm(Salary ~ ., data = Hitters, subset=train)
lm.pred <- predict(lm.fit, newdata = Hitters[-train, ])
lm_mse <- mean((salary.test - lm.pred)^2)
lm_mse #=0.49604

library(glmnet)

x <- model.matrix(Salary ~ ., Hitters)[, -1]
y <- Hitters$Salary

grid <- 10^seq(10, -2, length=100)
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

#cross-validation to computer associate test error
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[-train, ])
mean((lasso.pred - salary.test)^2) #=0.50802


boost.hitters <- gbm(Salary ~ ., data = Hitters, distribution = "gaussian", n.trees = 1000,
                     shrinkage = shrinkage[6])
summary(boost.hitters) #most important predictors: CAtBat, CHits, CRBI


#bagging test MSE
bag.hitters <- randomForest(Salary ~ ., data = Hitters, subset = train, mtry = length(Hitters)-1)
bag.pred <- predict(bag.hitters, Hitters[-train,])
mean((bag.pred - salary.test)^2) #=0.22186 MSE



#Caravan data set
rm(Caravan)
library(ISLR2)
Caravan[train, "Purchase"]

attach(Caravan)
train <- 1:1000
purchase.test <- Caravan[1001:nrow(Caravan), "Purchase"]

#convert "Yes" and "No" to 1 and 0 for bernoulli
purchase.test <- as.integer(ifelse(purchase.test == "No", 0, 1))
Caravan$Purchase <- as.integer(ifelse(Caravan$Purchase == "No", 0, 1))

purchase.test
Caravan$Purchase

names(Caravan)
Caravan <- subset(Caravan, select=-c(PVRAAUT,AVRAAUT))

caravan.boost <- gbm(Purchase ~ ., data = Caravan[train, ], n.trees = 1000, shrinkage = 0.01, distribution = "bernoulli")
caravan.boost

summary(caravan.boost) #most important variables PPERSAUT, MKOOPKLA, and MOPLHOOG

caravan.prob <- predict(caravan.boost, newdata = Caravan[1001:nrow(Caravan),], n.trees=1000, type = "response")
caravan.prob

#predict that a purchase will be made if prob>20%
caravan.pred <- rep(0, length(caravan.prob))
caravan.pred[caravan.prob > .2] <- 1

table(caravan.pred, purchase.test)
33/(131+33) #20.1% of predicted purchases end up in a purchase

#compare this to knn and logistic

#knn

names(Caravan) != "Purchase"

train.X <- cbind(Caravan[names(Caravan) != "Purchase"])[train,]
test.X <- cbind(Caravan[names(Caravan) != "Purchase"])[1001:nrow(Caravan),]
train.Purchase <- Purchase[train]
library(class)
knn.pred <- knn(train.X, test.X, train.Purchase)
table(knn.pred, purchase.test)
#accuracy
24/(270+24) #=8.16%

#use k = 3
knn.pred <- knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)
(48+87)/252 #=53.6%, alternatively mean(knn.pred == Direction.2005)


#logistic regression
glm.fits <- glm(Purchase ~ .,
                data = Caravan[train, ],
                family = binomial)
glm.prob <- predict(glm.fits, newdata = Caravan[1001:nrow(Caravan),], type="response")

glm.pred <- rep(0, length(glm.prob))
glm.pred[glm.prob > .2] <- 1

table(glm.pred, purchase.test)
58/(58+350) #14.2% of predicted purchases end up in a purchase


#apply boosting, bagging, random forests, BART to any data set
#fit models on training set and evaluate performance on a test set
#compare results to linear or logistic regression
#which is model has the best performance?

#use Auto data set
attach(Auto)
sum(is.na(Auto))
View(Auto)
Auto <- subset(Auto, select=-c(name))

#train-test split
set.seed(10)
train <- sample(1:nrow(Auto), nrow(Auto)/2)
auto.test <- Auto[-train, "mpg"]

#boosting
auto.boost <- gbm(mpg ~ ., data = Auto[train, ], distribution="gaussian")
auto.pred <- predict(auto.boost, newdata = Auto[-train, ])
mean((auto.pred - auto.test)^2) #= 8.4689 MSE

#bagging
auto.bag <- randomForest(mpg ~ ., data = Auto, subset = train, mtry = length(names(Auto))-1)
auto.pred <-predict(auto.bag, newdata = Auto[-train, ])
mean((auto.pred - auto.test)^2) #= 7.9455 MSE

#random forests
auto.rf <- randomForest(mpg ~ ., data = Auto, subset = train)
auto.pred <-predict(auto.rf, newdata = Auto[-train, ])
mean((auto.pred - auto.test)^2) #= 7.7932 MSE

#BART
x <- Auto[, names(Auto) != "mpg"]
y <- Auto[, "mpg"]
xtrain <- x[train,]
ytrain <- y[train]
xtest <- x[-train,]
ytest <- y[-train]

bartfit <- gbart(xtrain, ytrain, x.test = xtest)

yhat.bart <- bartfit$yhat.test.mean
mean((auto.test - yhat.bart)^2) #=7.3883 MSE

#linear regression
lm.fit <- lm(mpg ~ ., data = Auto, subset = train)
lm.pred <- predict(lm.fit, data = Auto[-train, ])
mean((lm.pred - auto.test)^2) #=109.74 MSE

#BART has the best performance at 7.79 MSE
#linear regression has the worst performance at 109.74 MSE
