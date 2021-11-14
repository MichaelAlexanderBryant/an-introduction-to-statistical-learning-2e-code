##8.3.1 Fitting Classification Trees

#library for constructing classification and regression trees
install.packages('tree')
library(tree)

#Carseats data set
library(ISLR2)
attach(Carseats)

#turn Sales into a categorical variable (if <= 8 then No, if >8 then Yes)
High <- factor(ifelse(Sales <= 8, "No", "Yes"))

#merge into dataframe
Carseats <- data.frame(Carseats, High)

#use tree function to fit a classification tree for High onto all variables except Sales
tree.carseats <- tree(High ~ . - Sales, Carseats)

#tree results
summary(tree.carseats) #error rate: 9%

#graphical display of tree
plot(tree.carseats) #structure of tree
text(tree.carseats, pretty = 0) #display names and values, where pretty=0 displays full category names 

#show tree in text form (with some additional information)
tree.carseats

#train-test split
set.seed(2)
train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train,]
High.test <- High[-train]

#fit model
tree.carseats <- tree(High ~ . - Sales, Carseats, subset = train)
#predict on test set, type = class instructs R to return the actual class prediction
tree.pred <- predict(tree.carseats, Carseats.test, type = 'class')
#confusion matrix
table(tree.pred, High.test)
#accuracy
(104 + 50)/200 #=77%

#cross-validation where FUN = ... indicates to use classification error rate rather than deviance
set.seed(7)
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)
cv.carseats #dev = CV error

par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type = "b") #cv error against tree size
plot(cv.carseats$k, cv.carseats$dev, type = "b") #cv error against cost-complexity parameter (alpha)

#prune the tree to nine-node tree (with lowest CV error)
prune.carseats <- prune.misclass(tree.carseats, best = 9)

#plot pruned tree
par(mfrow=c(1,1))
plot(prune.carseats)
text(prune.carseats, pretty = 0)

#predict with pruned tree
tree.pred <- predict(prune.carseats, Carseats.test, type = "class")
#confusion matrix
table(tree.pred, High.test)
#accuracy
(97+58)/200 #= 77.5%, slightly better accuracy and better interpretability from pruning

#increasing the size of the tree produces lower accuracy and worse interpretability
prune.carseats <- prune.misclass(tree.carseats, best = 14)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
tree.pred <- predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
(102+52)/200 #= 77%

##8.3.2 Fitting Regression Trees

#fit a regression tree using Boston data set
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston <- tree(medv ~ ., Boston, subset = train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston, pretty = 0)

#a larger tree can be made by passing the following argument into the tree() function:
#control = tree.control(nobs = Length(train), mindev = 0)

#cross validation for pruning
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = "b")

#7 leaves is best, but to prune a regression tree to 5 leaves
prune.boston <- prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)

#use unpruned tree to make predictions
yhat <- predict(tree.boston, newdata = Boston[-train,])
boston.test <- Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0, 1)
mean((yhat - boston.test)^2) #=35.29 MSE, RMSE = 5.941 ($5,941)

##8.3.3 Bagging and Random Forests

#download randomForest
urlPackage <- "https://cran.r-project.org/src/contrib/Archive/randomForest/randomForest_4.6-10.tar.gz"
install.packages(urlPackage, repos=NULL, type="source")
library(randomForest)

#bagging on Boston data set, mtry = 12 means all 12 predictors should be used for each split of the tree
set.seed(1)
bag.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 12, importance = T)
bag.boston

yhat.bag <- predict(bag.boston, newdata = Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag - boston.test)^2) #=23.50 MSE, much better than a single tree (35.29 MSE)

#change number of trees to 25
bag.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry  = 12, ntree = 25)
yhat.bag <- predict(bag.boston, newdata = Boston[-train,])
mean((yhat.bag - boston.test)^2) #=24.07 MSE

#random forest
#change to mtry=6 (default for regression = p/3, default for classification = sqrt(p))
set.seed(1)
rf.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 6, importance = T)
yhat.rf <- predict(rf.boston, newdata = Boston[-train,])
mean((yhat.rf - boston.test)^2) #=19.57 MSE, best MSE yet

#feature importance (rm and lstat are most important)
importance(rf.boston)
varImpPlot(rf.boston)

##8.3.4 Boosting

#boosting on Boston data set
install.packages('gbm')
library(gbm)

#distribution = "gaussian" for regression, distribution = "bernoulli" for classification
#5000 treesinteraction depth limits depth of each tree
set.seed(1)
boost.boston <- gbm(medv ~ ., data = Boston[train,], distribution = "gaussian", n.trees = 5000,
                    interaction.depth = 4)
summary(boost.boston) #feature importance (lstat and rm most important

#partial dependence plots
plot(boost.boston, i = "rm") #median house price increase with rm
plot(boost.boston, i = "lstat") #median house price decrease with lstat

#predict with boosted model
yhat.boost <- predict(boost.boston, newdata = Boston[-train,], n.trees = 5000)
mean((yhat.boost - boston.test)^2) #=18.39 MSE, best MSE yet

#boosting with different parameters
# shrinkage (lambda) is 0.001 default
boost.boston <- gbm(medv ~ ., data = Boston[train,],
                    distribution = "gaussian", n.trees = 5000,
                    interaction.depth = 4, shrinkage = 0.2,
                    verbose = F)
yhat.boost <- predict(boost.boston, newdata = Boston[-train,], n.trees = 5000)
mean((yhat.boost - boston.test)^2) #= 16.55 MSE, best MSE yet

##8.3.5 Bayesian Additive Regression Trees

#BART on Boston data set
urlPackage <- "https://cran.r-project.org/src/contrib/Archive/BART/BART_2.8.tar.gz"
install.packages(urlPackage, repos=NULL, type="source")
library(BART)

x <- Boston[, 1:12]
y <- Boston[, "medv"]
xtrain <- x[train,]
ytrain <- y[train]
xtest <- x[-train,]
ytest <- y[-train]

#use gbart() for quantitative outcome variables (lbart() or pbart() for binary outcomes)
set.seed(1)
bartfit <- gbart(xtrain, ytrain, x.test = xtest)

#test error
yhat.bart <- bartfit$yhat.test.mean
mean((ytest - yhat.bart)^2) #=15.95 MSE, best MSE

#how many times each variable appeared in the collection of trees
ord <- order(bartfit$varcount.mean, decreasing = T)
bartfit$varcount.mean[ord] #nox, lstat, tax, and rad appeared the most