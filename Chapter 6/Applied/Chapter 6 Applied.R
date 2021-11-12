set.seed(1)
X = rnorm(100)
eps = rnorm(100)
Y = 3 + 5*X + 2*X^2 + .5*X^3 + eps
data = data.frame(X,Y)

library(leaps)

#best subset selection
regfit.best <- regsubsets(Y ~ poly(X,10), data = data, nvmax = 10)

names(summary(regfit.best))
summary(regfit.best)$cp # best: 4 variable model
summary(regfit.best)$bic # best: 1 variable model
summary(regfit.best)$adjr2 # best: 6 variable model

#forward stepwise selection
regfit.fwd <- regsubsets(Y ~ poly(X,10), data = data, nvmax = 10, method="forward")

summary(regfit.fwd)$cp # best: 4 variable model
summary(regfit.fwd)$bic # best: 1 variable model
summary(regfit.fwd)$adjr2 # best: 5 variable model

#backward stepwise selection
regfit.bwd <- regsubsets(Y ~ poly(X,10), data = data, nvmax = 10, method="backward")

summary(regfit.bwd)$cp # best: 4 variable model
summary(regfit.bwd)$bic # best: 1 variable model
summary(regfit.bwd)$adjr2 # best: 5 variable model

#similar results for each subset selection method except for adj. R-squared

#fit a lasso model to the data
library(glmnet)

#create grid of values ranging from 10^10 to 10^-2
grid <- 10^seq(10, -2, length=100)

#put data in correct format
x_lasso <- model.matrix(Y ~ poly(X,10), data)[, -1]
y_lasso <- data$Y

#train-test split
train <- sample(1:nrow(x_lasso), nrow(x_lasso)/2)
test <- (-train)
y.test <- y_lasso[test]

#fit the model and plot
lasso.mod <- glmnet(x_lasso[train, ], y_lasso[train], alpha = 1, lambda = grid)
plot(lasso.mod)

#cross-validation to compute estimated test error
cv.out <- cv.glmnet(x_lasso[train, ], y_lasso[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x_lasso[test, ])
mean((lasso.pred - y.test)^2) #=2.32719

#three of the 10 coefficients are zero
out <- glmnet(x_lasso, y_lasso, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type="coefficients", s = bestlam)[1:11,]
lasso.coef




#new data
Y = 65 + 39*X^7 + eps
data = data.frame(X,Y)

#best subset selection
regfit.best <- regsubsets(Y ~ poly(X,10), data = data, nvmax = 10)

summary(regfit.best)$cp # best: 9 variable model
summary(regfit.best)$bic # best: 1 variable model
summary(regfit.best)$adjr2 # best: 7 to 10 variable models

#forward stepwise selection
regfit.fwd <- regsubsets(Y ~ poly(X,10), data = data, nvmax = 10, method="forward")

summary(regfit.fwd)$cp # best: 7 variable model
summary(regfit.fwd)$bic # best: 1 variable model
summary(regfit.fwd)$adjr2 # best: 7 to 10 variable model

#backward stepwise selection
regfit.bwd <- regsubsets(Y ~ poly(X,10), data = data, nvmax = 10, method="backward")

summary(regfit.bwd)$cp # best: 7 variable model
summary(regfit.bwd)$bic # best: 1 variable model
summary(regfit.bwd)$adjr2 # best: 7 to 10 variable model

#similar results for each subset selection method except for Cp

#fit a lasso model to the data

#put data in correct format
x_lasso <- model.matrix(Y ~ poly(X,10), data)[, -1]
y_lasso <- data$Y

#fit the model and plot
lasso.mod <- glmnet(x_lasso[train, ], y_lasso[train], alpha = 1, lambda = grid)
plot(lasso.mod)

#cross-validation to compute estimated test error
cv.out <- cv.glmnet(x_lasso[train, ], y_lasso[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x_lasso[test, ])
mean((lasso.pred - y.test)^2) #=17176272

#five of the 10 coefficients are zero
out <- glmnet(x_lasso, y_lasso, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type="coefficients", s = bestlam)[1:11,]
lasso.coef

#predict number of applications
library(ISLR2)

View(College)
sum(is.na(College))

set.seed(1)
train <- sample(1:nrow(College), nrow(College)/2)
test <- (-train)
y.test <- College$Apps[test]
attach(College)

#linear model
lm.fit <- lm(Apps ~ ., data = College, subset=train)
mean((Apps - predict(lm.fit, College))[test]^2) #=1135758 MSE

#ridge regression

#glmnet takes an x matrix and y vector
x <- model.matrix(Apps ~ ., College)[, -1]
y <- College$Apps

grid <- 10^seq(10, -2, length=100)

ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test, ])
mean((ridge.pred - y.test)^2) #=976647.8 MSE

#lasso regression
lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = grid, thresh = 1e-12)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test, ])
mean((ridge.pred - y.test)^2) #=976647.8 MSE

#PCR

library(pls)

pcr.fit <- pcr(Apps ~ ., data = College, subset = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type="MSEP") #lowest MSE occurs with 17 components

pcr.pred <- predict(pcr.fit, x[test, ], ncomp = 17)
mean((pcr.pred - y.test)^2) #=1135758

#PLS

pls.fit <- plsr(Apps ~ ., data = College, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type="MSEP") #lowest MSE is at M=11

pls.pred <- predict(pls.fit, x[test, ], ncomp=11)

mean((pls.pred - y.test)^2) #=1124003

#conclusion on the five approaches: Ridge and Lasso produced the "best" results,
#however, all errors were large and comparable in magnitude.

#exploring the concept that training error will decrease with number of features and
#test error may not

#simulated data set of 20 predictors, 1000 observation with Y = b*X + eps where
#some b are zero

set.seed(NULL)
x1 = rnorm(1000)
x2 = rnorm(1000)
x3 = rnorm(1000)
x4 = rnorm(1000)
x5 = rnorm(1000)
x6 = rnorm(1000)
x7 = rnorm(1000)
x8 = rnorm(1000)
x9 = rnorm(1000)
x10 = rnorm(1000)
x11 = rnorm(1000)
x12 = rnorm(1000)
x13 = rnorm(1000)
x14 = rnorm(1000)
x15 = rnorm(1000)
x16 = rnorm(1000)
x17 = rnorm(1000)
x18 = rnorm(1000)
x19 = rnorm(1000)
x20 = rnorm(1000)
eps = rnorm(1000)
Y = eps + 5*x1 + 0.3*x2 + x3 + 3*x4 + 0*x5 + 0.7*x6 + 0*x7 + x8 + 3*x9 + 0*x10 + x11 + x12 + .535*x13 + 0*x14 + .3*x15 + 0*x16 + x17 + 0*x18 + x19 + 3*x20

data = data.frame(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, Y)

set.seed(2)
train <- sample(1:nrow(data), 100) #100 train and 900 test
test <- (-train)
y.test <- data$Y[test]

regfit.best <- regsubsets(Y ~ ., data = data, nvmax = 19)

#build "X" matrix
train.mat <- model.matrix(Y ~ ., data = data[train, ])

#compute MSE
val.errors <- rep(NA, 19)
for (i in 1:19) {
  coefi <- coef(regfit.best, id = i)
  pred <- train.mat[, names(coefi)] %*% coefi
  val.errors[i] <- mean((data$Y[train] - pred)^2)
}

plot(val.errors, xlab="Number of Variables", ylab="Training MSE")

#build "X" matrix
test.mat <- model.matrix(Y ~ ., data = data[test, ])

#compute MSE
val.errors <- rep(NA, 19)
for (i in 1:19) {
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[, names(coefi)] %*% coefi
  val.errors[i] <- mean((data$Y[test] - pred)^2)
}

plot(val.errors, xlab="Number of Variables", ylab="Test MSE")

#results: training error steadily decreases with more variables and test error does not
#when I had more complex relationships, the test error went up while the training error went down
#the best model (according to the test error) is a three-variable model

#stopped at 10(g)

test.mat <- model.matrix(Y ~ ., data = data[test, ])
actual_coef = setNames(c(5, 0.3, 1, 3, 0, 0.7, 0, 1, 3, 0 ,1, 1, .535, 0, .3, 0, 1, 0, 1, 3),
                       c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20"))

coef_compare <- rep(NA, 19)

for (i in 1:19){
  coefi <- coef(regfit.best, id = i)
  best_coef <- coef(regfit.best, i)
  comparison <- na.omit(merge(x=data.frame(best_coef),y=data.frame(actual_coef), by=0, all.x=TRUE))
  coef_compare[i] <- sqrt(sum((comparison[,'best_coef'] - comparison[,'actual_coef'])^2))
}

plot(coef_compare)

#predict per capita crime rate

attach(Boston)
#use try best subset selection, lasso, ridge regression, and PCR
#present and discuss results

sum(is.na(Boston))

#best subset selection
regfit.best <- regsubsets(crim ~ ., data = Boston, nvmax = 13)

names(summary(regfit.best))
par(mfrow = c(2,2))
plot(summary(regfit.best)$cp) 
plot(summary(regfit.best)$bic) 
plot(summary(regfit.best)$adjr2)

which.min(summary(regfit.best)$cp)
which.min(summary(regfit.best)$bic)
which.max(summary(regfit.best)$adjr2)

coef(regfit.best,9) #non-zero variables: zn, nox, dis, rad, ptratio, lstat, medv

#lasso
#create grid of values ranging from 10^10 to 10^-2
grid <- 10^seq(10, -2, length=100)

#put data in correct format
x_lasso <- model.matrix(crim ~ ., Boston)[, -1]
y_lasso <- Boston$crim

#train-test split
train <- sample(1:nrow(x_lasso), nrow(x_lasso)/2)
test <- (-train)
y.test <- y_lasso[test]

library(glmnet)

#fit the model and plot
par(mfrow=c(1,1))
lasso.mod <- glmnet(x_lasso[train, ], y_lasso[train], alpha = 1, lambda = grid)
plot(lasso.mod)

#cross-validation to compute estimated test error
cv.out <- cv.glmnet(x_lasso[train, ], y_lasso[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x_lasso[test, ])
mean((lasso.pred - y.test)^2) #=26.3345

#three of the 10 coefficients are zero
out <- glmnet(x_lasso, y_lasso, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type="coefficients", s = bestlam)[1:13,]
lasso.coef #non-zero variables: zn, indus, chas, nox, rm, dis, rad, ptratio, lstat, medv



#ridge regression

#put data in correct format
x_ridge <- model.matrix(crim ~ ., Boston)[, -1]
y_ridge <- Boston$crim

#train-test split
train <- sample(1:nrow(x_ridge), nrow(x_ridge)/2)
test <- (-train)
y.test <- y_ridge[test]

#fit the model and plot
par(mfrow=c(1,1))
ridge.mod <- glmnet(x_ridge[train, ], y_ridge[train], alpha = 0, lambda = grid)
plot(ridge.mod)

#cross-validation to compute estimated test error
cv.out <- cv.glmnet(x_ridge[train, ], y_ridge[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x_ridge[test, ])
mean((ridge.pred - y.test)^2) #=32.56685 (worse than lasso)

out <- glmnet(x_ridge, y_ridge, alpha = 0, lambda = grid)
ridge.coef <- predict(out, type="coefficients", s = bestlam)[1:13,]
ridge.coef 


#PCR

library(pls)

pcr.fit <- pcr(crim ~ ., data = Boston, subset = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type="MSEP") #lowest MSE occurs with 12 components
names(pcr.fit)
summary(pcr.fit)$scores

x <- model.matrix(crim ~ ., Boston)[, -1]

pcr.pred <- predict(pcr.fit, x[test, ], ncomp = 12)
mean((pcr.pred - y.test)^2) #=32.60272 slightly worse than ridge


#the best model is the lasso which excludes age (12 predictor model), the best subset selection also excluded age,
#and ridge regression made the coefficient for age the smallest