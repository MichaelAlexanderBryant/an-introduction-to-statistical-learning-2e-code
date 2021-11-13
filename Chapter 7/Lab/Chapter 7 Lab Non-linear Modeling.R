library(ISLR2)
attach(Wage)

##7.8.1 Polynomial Regression and Step Functions
fit <- lm(wage ~ poly(age,4), data = Wage)
coef(summary(fit))

#poly() returns a matrix whose columns are a basis of orthogonal polynomials
#to obtain age, age^2, age^3, age^4 directly, use raw=TRUE

fit2 <- lm(wage ~ poly(age, 4, raw=T), data = Wage)
coef(summary(fit2)) #same as previous fit, affects coef estimates but not fitted values

#alternative to poly(), I() is a wrapper, cbind() is another alternative
fit2a <- lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data = Wage)
coef(fit2a)

#create a grid of values for age and predict
agelims <- range(age)
age.grid <- seq(from = agelims[1], to = agelims[2])
preds <- predict(fit, newdata = list(age=age.grid), se=TRUE)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit) #standard error bands

#plot
par(mfrow = c(1,2), mar = c(4.5, 4.5, 1, 1), oma=c(0,0,4,0)) #mar and oma are for controlling margins
plot(age, wage, xlim=agelims, cex=.5, col = "darkgrey")
title("Degree-4 Polynomial", outer=T)
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)

#demonstrating different poly() functions are equivalent
preds2 <- predict(fit2, newdata = list(age=age.grid), se=TRUE)
max(abs(preds$fit - preds2$fit)) #= 7.81597e-11

#use anova with an F-test to test the hypothesis that the simpler null model is sufficient to explain
#the data against a more complex model (the alternative model) where the predictors in the null model
#are a subset of the predictors of the alternative model

fit.1 <- lm(wage ~ age, data = Wage)
fit.2 <- lm(wage ~ poly(age,2), data = Wage)
fit.3 <- lm(wage ~ poly(age,3), data = Wage)
fit.4 <- lm(wage ~ poly(age,4), data = Wage)
fit.5 <- lm(wage ~ poly(age,5), data = Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

#comparing model 2 to model 1: p-value<2.2e-16 meaning model 1 is not sufficient
#comparing model 2 to model 3: p-value=0.001679 meaning model 2 is not sufficient
#all other fits have too large of p-values, so a quadratic or cubic fit is best

#alternatively to above, look at p-values for t-values of coefficients
coef(summary(fit.5))

#square of the t-statistics are equal to the F-statistics
#this only works with orthogonal polynomials, use anova() in general

#for example, anova is necessary for the below comparisons
fit.1 <- lm(wage ~ education + age, data = Wage)
fit.2 <- lm(wage ~ education + poly(age,2), data = Wage)
fit.3 <- lm(wage ~ education + poly(age,3), data = Wage)
anova(fit.1, fit.2, fit.3)

#alternatively, cross-validation can be used to compare fits

#polynomial logistic regression (pr(wage >$250k/yr | predictors))
fit <- glm(I(wage >250) ~ poly(age,4), data = Wage, family=binomial)
preds <- predict(fit, newdata = list(age = age.grid), se=T)

#uses log-odds form (default: type = "link")
#to calculate confidence interval, a transformation is needed

pfit <- exp(preds$fit) / (1 + exp(preds$fit))
se.bands.logit <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)

#alternatively to the above, type = "response"

preds <- predict(fit, newdata = list(age=age.grid), type="response", se=T)

#however, the confidence interval doesn't make sense due to negative values

plot(age, I(wage>250), xlim=agelims, type = "n", ylim=c(0,.2))
points(jitter(age), I((wage>250)/5), cex=.5, pch="|", col="darkgrey")
lines(age.grid, pfit, lwd=2, col="blue")
matlines(age.grid, se.bands,lwd=1, col="blue", lty=3)

#step function
table(cut(age,4))
fit <- lm(wage ~cut(age,4), data = Wage)
coef(summary(fit))

#age <33.5 was left out, so intercept can be interpreted as those under 33.5 make $94,158/yr

##7.8.2 Splines

#fit wage to age using regression spline
library(splines)
fit <- lm(wage ~ bs(age, knots = c(25,40,60)), data = Wage) #bs() is a basis function
pred <- predict(fit, newdata=list(age=age.grid), se=T)
plot(age, wage, col="gray")
lines(age.grid, pred$fit, lwd=2)
lines(age.grid, pred$fit + 2*pred$se, lty="dashed")
lines(age.grid, pred$fit - 2*pred$se, lty="dashed")
#knots at ages 25, 40, and 60

dim(bs(age, knots=c(25,40,60)))
dim(bs(age, df=6)) #degrees of freedom (df), cubic spline with three knots has seven degrees of freedom and one intercept (six basis functions)
attr(bs(age, df=6), "knots") #knots at uniform quantiles (25th, 50th, 75th)

#natural spline of four degrees of freedom
fit2 <- lm(wage ~ns(age, df=4), data = Wage)
pred2 <- predict(fit2, newdata = list(age = age.grid), se=T)
lines(age.grid,pred2$fit, col = "red", lwd=2)

#smoothing spline
plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
title("Smoothing Spline")
fit <- smooth.spline(age, wage, df=16) #lambda determined from 16 DF
fit2 <- smooth.spline(age, wage, cv=TRUE) #lambda determined through cross-validation
fit2$df #=6.8
lines(fit, col="red", lwd=2)
lines(fit2, col="blue", lwd=2)
legend("topright", legend=c("16 DF", "6.8 DF"), col=c("red", "blue"), lty=1, lwd=2, cex=.8)

#local regression
plot(age, wage, xlim = agelims, cex = .5, col="darkgrey")
title("Local Regression")
fit <- loess(wage~age, span=.2, data = Wage)
fit2 <- loess(wage~age, span=.5, data = Wage)
lines(age.grid, predict(fit, data.frame(age=age.grid)), col="red", lwd=2)
lines(age.grid, predict(fit2, data.frame(age=age.grid)), col="blue", lwd=2)
legend("topright", legend = c("Span = 0.2", "Span = 0.5"), col=c("red","blue"), lty=1, lwd=2, cex=.8)

##7.8.3 GAMs

#use GAM to predict wage using natural splines for year and age, and treating education as a qualitative variable
gam1 <- lm(wage ~ ns(year,4) + ns(age, 5) + education, data = Wage)

#use gam() with smoothing splines (s())
install.packages('gam')
library(gam)

#year has four DF, age has five DF
gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)

#plot recognizes Gam class and plots plot.Gam()
par(mfrow = c(1,3))
plot(gam.m3, se=TRUE, col = "blue")

#if using the previous lm method
plot.Gam(gam1, se=T, col = "red")

#use ANOVA tests to find whether the model should (1) exlude year, (2) include it as a linear function,
#or (3) use a spline function

gam.m1 <- gam(wage ~ s(age, 5) + education, data = Wage)
gam.m2 <- gam(wage ~ year + s(age,5) + education, data = Wage)
anova(gam.m1, gam.m2, gam.m3, test= "F")

#results indicate that a linear function of year is preferred

summary(gam.m3) #year p-value is 0.35, age p-value <2e-16 (Anova for Nonparametric effects)

#make predictions with model containing linear function of year
preds <- predict(gam.m2, newdata=Wage)

#use local regression as building blocks in a GAM
gam.lo <- gam(wage~ s(year, df=4) + lo(age, span=0.7) + education, data = Wage)
plot.Gam(gam.lo, se=T, col="green")

#use local regression with a year and age interaction term
gam.lo.i <- gam(wage ~ lo(year, age, span = 0.5) + education, data = Wage)

#plot the two-dimensional surface
install.packages('akima')
library(akima)
plot(gam.lo.i)

#logistic regression GAM
gam.lr <- gam(I(wage > 250) ~ year + s(age, df=5) + education, family = binomial, data = Wage)
par(mfrow = c(1,3))
plot(gam.lr,se=T, col="green")

#no high earners in the <HS education category
table(education, I(wage > 250))

#fit logistic regression GAM to all but this category
gam.lr.s <- gam(I(wage > 250) ~ year + s(age, df=5) + education,
                family = binomial,
                data = Wage,
                subset = (education != "1. < HS Grad"))
plot(gam.lr.s, se = T, col = "green")
