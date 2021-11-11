##6.5.1 Subset Selection Methods

#types of subset selection: best, forward stepwise, backward stepwise

library(ISLR2)
View(Hitters)
names(Hitters)
dim(Hitters)

#number of missing values
sum(is.na(Hitters$Salary))

#omit missing values
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

#best subset selection
library(leaps)
regfit.full <- regsubsets(Salary ~ ., Hitters)
summary(regfit.full) #asterisk  indicates a variable is included in the corresponding model
#8 models ranging from 1 to 8 variables

#nvmax option can be used to fit up to 19-variable model
regfit.full <- regsubsets(Salary ~ .,
                          data = Hitters,
                          nvmax = 19)

#summary also contains metrics
reg.summary <- summary(regfit.full)
names(reg.summary)

#r-squared for 19 models, increases with more variables (expected)
reg.summary$rsq

#2x2 grid
par(mfrow = c(2,2))

#plot RSS
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS")
lines(reg.summary$rss)

#plot adj. r-squared
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adj. R-Squared")
lines(reg.summary$adjr2)
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col='red', cex = 2, pch = 20)

#plot Cp
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp")
lines(reg.summary$cp)
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col='red', cex=2, pch=20)

#plot BIC
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC")
lines(reg.summary$bic)
which.min(reg.summary$bic)
points(6, reg.summary$bic[6], col='red', cex=2, pch=20)

#regsubplot built in plot function
#top rows indicate which variables (black squares) make up the best model
par(mfrow = c(1,1))
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")

#coefficients of the best model (in this case with 6 variables)
coef(regfit.full, 6)

#forward and backward stepwise selection
regfit.fwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "forward")
summary(regfit.fwd)
regfit.bwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "backward")
summary(regfit.bwd)

#coefficients for models with seven variables (demonstrating subset selection results in different models)
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

#choosing a model using validation set and cross-validation
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(Hitters), replace = TRUE)
test <- !train

regfit.best <- regsubsets(Salary ~ ., data = Hitters[train, ], nvmax = 19)

#build "X" matrix
test.mat <- model.matrix(Salary ~ ., data = Hitters[test, ])

#compute MSE
val.errors <- rep(NA, 19)
for (i in 1:19) {
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[, names(coefi)] %*% coefi
  val.errors[i] <- mean((Hitters$Salary[test] - pred)^2)
}

#seven variable model has the lowest MSE (MSE = 126849)
val.errors
which.min(val.errors)
coef(regfit.best, 7)

#this was tedious because no predict method exists for regsubsets
#create own predict function:
predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id=id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

#comparison to using the full data set (demonstrates different model)
regfit.best <- regsubsets(Salary ~., data = Hitters, nvmax = 19)
coef(regfit.best, 7)

#10 fold cross validation
k <- 10
n <- nrow(Hitters)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))
#j are in the test set
for (j in 1:k) {
  best.fit <- regsubsets(Salary ~ .,
                         data = Hitters[folds != j, ],
                         nvmax = 19)
  for (i in 1:19) {
    pred <- predict(best.fit, Hitters[folds == j, ], id = 1)
    cv.errors[j, i] <- mean((Hitters$Salary[folds == j] - pred)^2)
  }
}
#apply to average over columns of 10x19 matrix to produce a vector where the ith element is the cv error for the i-variable model
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors #10-variable model is best

par(mfrow = c(1,1))
plot(mean.cv.errors, type = "b")

#create a 10 variable model using full data set
reg.best <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
coef(reg.best, 10)

##6.5.2 Ridge Regression and the Lasso

#number of missing values
sum(is.na(Hitters$Salary))

#omit missing values
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

#glmnet takes an x matrix and y vector
x <- model.matrix(Salary ~ ., Hitters)[, -1]
y <- Hitters$Salary

install.packages('glmnet')
library(glmnet)

#create grid of values ranging from 10^10 to 10^-2
grid <- 10^seq(10, -2, length=100)
#ridge regression model (alpha = 0) (lasso is alpha = 1)
#variables are automatically standardized (standardize = FALSE to turn off)
ridge.mod <- glmnet(x,y,alpha = 0, lambda = grid)

#20x100 matrix (20 predictors, 100 lambda values)
dim(coef(ridge.mod))

#coef are expected to be smaller for larger values of lambda
ridge.mod$lambda[50] #=11498
coef(ridge.mod)[, 50]
#for comparison
ridge.mod$lambda[60] #=705
coef(ridge.mod)[, 60] #larger coefficients
sqrt(sum(coef(ridge.mod)[-1,60]^2)) #=57.1

#predict can be used to obtain ridge regression coefficients for a new value of lambda (e.g. 50)
predict(ridge.mod, s = 50, type = "coefficients")[1:20,]

#train-test split (alternative method to TRUE/FALSE method used before)
#randomly choose a subset of numbers between 1 and n
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

#fit ridge regression model
ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
#predict (where newx is used to get predictions from test set intstead of type="coefficients")
ridge.pred <- predict(ridge.mod, s=4, newx = x[test,])
mean((ridge.pred - y.test)^2) #=142199

#had the model been fit with just an intercept (null model) then test set MSE is computed by: mean(mean(y[train] - y.test)^2) = 224670

#larger lambda produces same result as model fit with just intercept
ridge.pred <- predict(ridge.mod, s=1e10, newx =x[test,])
mean((ridge.pred - y.test)^2) #= 224670

#therefore a ridge model with lambda = 4 has a much lower test MSE than fitting a model with just an intercept

#check if there is a benefit of ridge over least squares regression (i.e., lambda = 0)
ridge.pred <- predict(ridge.mod, s =0, newx = x[test, ], exact = T, x = x[train, ], y = y[train])
mean((ridge.pred - y.test)^2) #=168588, ridge is better

#use cross-validation to select lambda instead of arbitrarily choosing lambda
#done using cv.glmnet(), nfolds=10 by default
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha=0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam #=326 = log(5.78)

#test MSE associated with lambda = 326
ridge.pred <- predict(ridge.mod, s=bestlam, newx = x[test,])
mean((ridge.pred - y.test)^2) #=139857

#refit model with full data set with lambda = 326
out <- glmnet(x,y,alpha=0)
predict(out, type="coefficients", s=bestlam)[1:20, ]

#fit lasso model (alpha = 1)
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

#cross-validation to computer associate test error
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test, ])
mean((lasso.pred - y.test)^2) #=143674, lower than null model and least squares / similar to ridge

#eight of the 19 variables are zero
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type="coefficients", s = bestlam)[1:20,]
lasso.coef

##6.5.3 PCR and PLS Regression

#number of missing values
sum(is.na(Hitters$Salary))

#omit missing values
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

install.packages('pls')
library(pls)
set.seed(2)
pcr.fit <- pcr(Salary ~ ., data = Hitters, scale = TRUE, validation = "CV") #scale means standardize, validation means 10-fold cv
#RMSE for models containing 0 to 19 components and percent of variance explained (e.g. 1 component explains 38.31% of variance)
summary(pcr.fit)

#plot cv MSE for the inclusion of each component
validationplot(pcr.fit, val.type="MSEP")

set.seed(1)
x <- model.matrix(Salary ~ ., Hitters)[, -1]
y <- Hitters$Salary
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]
pcr.fit <- pcr(Salary ~ ., data = Hitters, subset = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type="MSEP") #lowest MSE occurs with 5 components

pcr.pred <- predict(pcr.fit, x[test, ], ncomp = 5)
mean((pcr.pred - y.test)^2) #=142812, similar result to ridge and lasso, but more difficult to interpret

#use full data set to fit model
pcr.fit <- pcr(y~x, scale = TRUE, ncomp = 5)
summary(pcr.fit)

#fit partial least squares model
set.seed(1)
pls.fit <- plsr(Salary ~ ., data = Hitters, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type="MSEP") #lowest MSE is at M=1

pls.pred <- predict(pls.fit, x[test, ], ncomp=1)

mean((pls.pred - y.test)^2) #=151995 comparable to ridge, lasso, and PCR, but slightly higher

pls.fit <- plsr(Salary ~ ., data = Hitters, scale = TRUE, ncomp = 1)
summary(pls.fit) #pls 1 component explains 43.05% of variance whereas pcr 5 components explains 44.90%
#this is because PCR only attempts to maximize the amount of variance explained in the predictors while PLS
#searches for directions that explain variance in both the predictors and the response
#Principal component analysis = unsupervised, partial least square = supervised