library(ISLR2)

sum(is.na(Wage))

#perform polynomial regression to predict wage using age
#1. use CV to find the optimal degree
#2. use hypothesis testing (ANOVA)
#compare the two results

set.seed(1)

#CV
library(boot)
cv.error.10 <- rep(0,5)

for (i in 1:5) {
  glm.fit <- glm(wage ~ poly(age, i), data = Wage)
  #k = 10 (10-fold cross validation)
  cv.error.10[i] <- cv.glm(Wage, glm.fit, K = 10)$delta[1]
}

cv.error.10 #cubic and higher has lowest error

#ANOVA
lm.fit1 <- lm(wage ~ poly(age,1), data = Wage)
lm.fit2 <- lm(wage ~ poly(age,2), data = Wage)
lm.fit3 <- lm(wage ~ poly(age,3), data = Wage)
lm.fit4 <- lm(wage ~ poly(age,4), data = Wage)
lm.fit5 <- lm(wage ~ poly(age,5), data = Wage)
anova(lm.fit1, lm.fit2, lm.fit3, lm.fit4, lm.fit5)
#cubic is the highest order that is statistically significant

#plot third-degree fit
attach(Wage)
agelims <- range(age)
age.grid <- seq(from = agelims[1], to = agelims[2])
preds <- predict(lm.fit3, newdata = list(age=age.grid), se=TRUE)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

plot(age, wage, xlim=agelims, cex=.5, col = "darkgrey")
title("Degree-3 Polynomial", outer=T)
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)

#fit a step function to predict wage using age and perform cross-validation to choose the optimal number of cuts
#then plot
cv.error.10 <- rep(0,9)

for (i in 2:10) {
  Wage$age.cut <- cut(Wage$age, i)
  glm.fit <- glm(wage ~ age.cut, data = Wage)
  #k = 10 (10-fold cross validation)
  cv.error.10[i-1] <- cv.glm(Wage, glm.fit, K = 10)$delta[1]
}

cv.error.10

step.fit <- glm(wage~cut(age, 8), data=Wage)
agelims <- range(Wage$age)
age.grid <- seq(from=agelims[1], to=agelims[2])
step.pred <- predict(step.fit, data.frame(age=age.grid))
plot(wage~age, data=Wage, col="darkgrey")
lines(age.grid, step.pred, col="red", lwd=2)


#use non-linear modeling to explore other features such as maritl and jobclass
#create plots and summarize findings

names(Wage)

plot(wage~maritl, data=Wage, col="darkgrey")
plot(wage~jobclass, data=Wage, col="darkgrey")
plot(wage~race, data=Wage, col="darkgrey")
plot(wage~education, data=Wage, col="darkgrey")
plot(wage~region, data=Wage, col="darkgrey")
plot(wage~health, data=Wage, col="darkgrey")
plot(wage~health_ins, data=Wage, col="darkgrey")
plot(wage~logwage, data=Wage, col="darkgrey")
plot(wage~as.factor(year), data=Wage, col="darkgrey")


set.seed(1)
cv.error.10 <- rep(0,10)

for (i in 1:10) {
  glm.fit <- glm(wage ~ poly(age, i) + maritl + jobclass + race + education + health + health_ins, data = Wage)
  #k = 10 (10-fold cross validation)
  cv.error.10[i] <- cv.glm(Wage, glm.fit, K = 10)$delta[1]
}

cv.error.10 #cubic and higher has lowest error

plot(cv.error.10)
lines(cv.error.10)

#auto
attach(Auto)
names(Auto)
plot(mpg ~ as.factor(cylinders))
plot(mpg ~ displacement)
plot(mpg ~ horsepower)
plot(mpg ~ weight)
plot(mpg ~ acceleration)
plot(mpg ~ as.factor(year))
plot(mpg ~ as.factor(origin))

set.seed(1)

#polynomial fit
cv.error.10 <- rep(0,15)

for (i in 1:15) {
  glm.fit <- glm(mpg ~ poly(displacement, i), data = Auto)
  #k = 10 (10-fold cross validation)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}

cv.error.10 #cubic and higher has lowest error
plot(cv.error.10)
lines(cv.error.10)

glm.dis <- glm(mpg ~ poly(displacement, 10), data = Auto)

dislims <- range(displacement)
dis.grid <- seq(from = dislims[1], to = dislims[2])
preds <- predict(glm.dis, newdata = list(displacement = dis.grid), se=TRUE)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

plot(displacement, mpg, xlim=dislims, cex=.5, col = "darkgrey")
title("Degree-10 Polynomial", outer=T)
lines(dis.grid, preds$fit, lwd=2, col="blue")
matlines(dis.grid, se.bands, lwd=1, col="blue", lty=3)

#natural spline of four degrees of freedom
library(splines)
fit2 <- lm(mpg ~ns(displacement, df=5), data = Auto)
pred2 <- predict(fit2, newdata = list(displacement = dis.grid), se=T)
lines(dis.grid,pred2$fit, col = "green", lwd=2)

cv.error.10 <- rep(0,20)

for (i in 1:20) {
  glm.fit <- glm(mpg ~ ns(displacement, i), data = Auto)
  #k = 10 (10-fold cross validation)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}

cv.error.10
plot(cv.error.10)
lines(cv.error.10)

glm.ns <- glm(mpg ~ ns(displacement, df=12), data = Auto)

dislims <- range(displacement)
dis.grid <- seq(from = dislims[1], to = dislims[2])
preds <- predict(glm.ns, newdata = list(displacement = dis.grid), se=TRUE)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

plot(displacement, mpg, xlim=dislims, cex=.5, col = "darkgrey")
title("Natural-spline 12 DF", outer=T)
lines(dis.grid, preds$fit, lwd=2, col="blue")
matlines(dis.grid, se.bands, lwd=1, col="blue", lty=3)

#boston data set
attach(Boston)

sum(is.na(Boston))
length(dis)
length(Boston$nox)

lm.fit <- lm(nox ~ poly(dis,3), data = Boston)
summary(lm.fit)

dislims <- range(dis)
dis.grid <- seq(from = dislims[1], to = dislims[2])
preds <- predict(lm.fit, newdata = list(dis = dis.grid), se=TRUE)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

plot(dis, Boston$nox, xlim=dislims, cex=.5, col = "darkgrey")
title("Degree-3 Poly fit")
lines(dis.grid, preds$fit, lwd=2, col="blue")
matlines(dis.grid, se.bands, lwd=1, col="blue", lty=3)

cv.error.10 <- rep(0,10)

for (i in 1:10) {
  glm.fit <- glm(nox ~ poly(dis, i), data = Boston)
  #k = 10 (10-fold cross validation)
  cv.error.10[i] <- cv.glm(Boston, glm.fit, K = 10)$delta[1]
  
  preds <- predict(glm.fit, newdata = list(dis = dis.grid), se=TRUE)
  se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
  
  plot(dis, Boston$nox, xlim=dislims, cex=.5, col = "darkgrey")
  title(i)
  lines(dis.grid, preds$fit, lwd=2, col="blue")
  matlines(dis.grid, se.bands, lwd=1, col="blue", lty=3)
  
}

cv.error.10 #cubic and higher has lowest error
plot(cv.error.10)
which.min(cv.error.10)

#use bs() to fit a regression spline using 4 DF, choose knots, plot fit
glm.fit.q <- glm(nox ~ bs(dis, df=4), data = Boston)

preds <- predict(glm.fit.q, newdata = list(dis = dis.grid), se=TRUE)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

plot(dis, Boston$nox, xlim=dislims, cex=.5, col = "darkgrey")
title("Regression spline 4 DF using Quartiles")
lines(dis.grid, preds$fit, lwd=2, col="blue")
matlines(dis.grid, se.bands, lwd=1, col="blue", lty=3)

#using knots at areas of greatest change (4 DF)
dim(bs(dis, knots=c(5)))
glm.fit.c <- glm(nox ~ bs(dis, knots = c(5)), data = Boston)

preds <- predict(glm.fit.c, newdata = list(dis = dis.grid), se=TRUE)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

plot(dis, Boston$nox, xlim=dislims, cex=.5, col = "darkgrey")
title("Regression spline 4 DF using Custom Knots")
lines(dis.grid, preds$fit, lwd=2, col="blue")
matlines(dis.grid, se.bands, lwd=1, col="blue", lty=3)

#fit a regression spline for multiple DF, calculate RSS, plot fits

cv.error.10 <- rep(0,10)


for (i in 1:10) {
  glm.fit <- glm(nox ~ bs(dis, df=i), data = Boston)
  #k = 10 (10-fold cross validation)
  cv.error.10[i] <- cv.glm(Boston, glm.fit, K = 10)$delta[1]
  
  preds <- predict(glm.fit, newdata = list(dis = dis.grid), se=TRUE)
  se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
  
  plot(dis, Boston$nox, xlim=dislims, cex=.5, col = "darkgrey")
  title(i)
  lines(dis.grid, preds$fit, lwd=2, col="blue")
  matlines(dis.grid, se.bands, lwd=1, col="blue", lty=3)
  
}

plot(cv.error.10)
lines(cv.error.10)

#five DF had the lowest 10-fold CV error
glm.best <- glm(nox ~ bs(dis, df=5), data = Boston)

preds <- predict(glm.best, newdata = list(dis = dis.grid), se=TRUE)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

plot(dis, Boston$nox, xlim=dislims, cex=.5, col = "darkgrey")
title("Regression spline 5 DF using Quartiles")
lines(dis.grid, preds$fit, lwd=2, col="blue")
matlines(dis.grid, se.bands, lwd=1, col="blue", lty=3)

#College data set
#train-test split, forward stepwise selection on training set
#response: out-of-state tuition, predictors: all others
attach(College)
names(College)

train <- sample(1:nrow(College), nrow(College)/2)
test <- (-train)
y.test <- College$Outstate[test]


library(leaps)
regfit.fwd <- regsubsets(Outstate ~ ., data = College, nvmax = 19, method = "forward", subset=train)
summary(regfit.fwd)

par(mfrow=c(1,3))
plot(summary(regfit.fwd)$cp) # best: 9 variable model
x = which.min(summary(regfit.fwd)$cp)
y = summary(regfit.fwd)$cp[x]
points(x,y, col='red', cex = 2, pch = 20)
x

plot(summary(regfit.fwd)$bic) # best: 1 variable model
x = which.min(summary(regfit.fwd)$bic)
y = summary(regfit.fwd)$bic[x]
points(x,y, col='red', cex = 2, pch = 20)
x

plot(summary(regfit.fwd)$adjr2) # best: 7 to 10 variable models
x = which.max(summary(regfit.fwd)$adjr2)
y = summary(regfit.fwd)$adjr2[x]
points(x,y, col='red', cex = 2, pch = 20)
x

coef(regfit.fwd, 9)

gam.fit <- glm(Outstate ~ Private + Apps + Accept + Enroll + Top10perc + Room.Board + perc.alumni + Expend + Grad.Rate, data = College, subset=train)
summary(gam.fit)
par(mfrow = c(1, 1))
plot(gam.fit, se = T, col = "blue")

gam.pred <- predict(gam.fit, College)[test]
mean((y.test - gam.pred)^2) #=4258554 

summary(gam.fit)

#slight non-linear relationships
plot(Outstate ~ Apps)
plot(Outstate ~ Enroll)
plot(Outstate ~ Top10perc)
plot(Outstate ~ Grad.Rate)

#perform multiple linear regression by backfitting
set.seed(NULL)
x1 <- rnorm(100)
x2 <- rnorm(100)
eps <- rnorm(100)
y <- 3 + 500*x1 - 355*x2 + eps
beta1 <- 5

beta2.f <- rep(0,1000)
beta1.f <- rep(0,1000)
beta0.f <- rep(0,1000)

for (i in 1:1000){
  
  a <- y - beta1 * x1 
  beta2 <- lm(a ~ x2)$coef[2]
  beta2.f[i] <- beta2
  
  a <- y - beta2*x2
  beta1 <- lm(a ~ x1)$coef[2]
  beta1.f[i] <- beta1
  
  beta0 <- y - beta2*x2 - beta1*x1
  beta0.f[i] <- beta0
  
}

par(mfrow=c(1,1))
lm.fit <- lm(y ~ x1 + x2)

par(mfrow=c(1,3))
plot(beta2.f)
points(1000,lm.fit$coefficients[3], col=2, cex = 2, pch = 20)
plot(beta1.f)
points(1000,lm.fit$coefficients[2], col=2, cex = 2, pch = 20)
plot(beta0.f)
points(1000,lm.fit$coefficients[1], col=2, cex = 2, pch = 20)

#backfitting took three iterations to get a good estimate

#use backfitting to on p=100

set.seed(1)
p = 100
n = 1000
x = matrix(ncol = p, nrow = n)
coefi = rep(NA, p)
for (i in 1:p) {
  x[, i] = rnorm(n)
  coefi[i] = rnorm(1) * 100
}
y = x %*% coefi + rnorm(n)

beta = rep(0, p)
max_iterations = 1000
errors = rep(NA, max_iterations + 1)
iter = 2
errors[1] = Inf
errors[2] = sum((y - x %*% beta)^2)
threshold = 1e-04
while (iter < max_iterations && errors[iter - 1] - errors[iter] > threshold) {
  for (i in 1:p) {
    a = y - x %*% beta + beta[i] * x[, i]
    beta[i] = lm(a ~ x[, i])$coef[2]
  }
  iter = iter + 1
  errors[iter] = sum((y - x %*% beta)^2)
  print(c(iter - 2, errors[iter - 1], errors[iter]))
}

plot(1:11, errors[3:13])
