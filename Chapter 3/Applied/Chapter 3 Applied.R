#function to load the libraries for this book
LoadLibraries <- function() {
  library(ISLR2)
  library(MASS)
  print("The libraries have been loaded.")
}

#load the libraries
LoadLibraries()

#using the auto data set
attach(Auto)

#linear model for regressing mpg onto horsepower
lm.fit <- lm(mpg ~ horsepower)

#summary of the model
summary(lm.fit)

# 1. Is there a relationship between the predictor and the response?
#         Yes, the probability of obtaining a t-statistic for the coefficient of
#         24.49 given the null hypothesis is true is <2e-16.
#
# 2. How strong is the relationship betqqween the predictor and the response?
#         RSE = 4.906; the predicted values are off by 4.906 mpg.
#         R-squared is 0.6059; without domain knowledge I can't say how good or
#         bad this is. The model explains 60.59% of the variance.

#looking at the residual plot for question 2
plot(lm.fit$residuals, ylab='Residuals')

# 3. Is the relationship between the predictor and the response positive or negative?
#         Negative.
#
# 4a. What is the predicted mpg associated with a horsepower of 98?
#         24.47 mpg

#prediction for 4a
predict(lm.fit, data.frame(horsepower = 98))

# 4b. What are the associated 95 % confidence and prediction intervals?
#       confidence: [23.97, 24.96]
#       prediction: [14.81, 34.12]

#interval calculations
predict(lm.fit, data.frame(horsepower = 98),
        interval = "confidence")
predict(lm.fit, data.frame(horsepower = 98),
        interval = "prediction")

#mpg vs horsepower with ols line
plot(horsepower, mpg, col = 4)
abline(lm.fit, col=2, lwd=3)

#residuals vs fitted plot shows non-linearity (diagnostic plots)
par(mfrow = c(2,2))
plot(lm.fit)

#scatterplot matrix of all variables
pairs(Auto, col=2)

#list of variable names
names(Auto[ ,-9])

#correlation matrix except name (categorical)
cor(Auto[ ,-9])

lm.fit <- lm(mpg ~ . - name, data = Auto)
summary(lm.fit)

# There is a relationship between the predictors and response. The F-statistic
# is 252.4 which is much greater than 1 and the p-value is <2.2e-16.

# Predictors with statisticall signficant relationship: displacement, weight,
# year, and origin

# The coefficient for the year is 0.750773 which suggests the mpg increases
# by this amount for every year

#diagnostic plots
par(mfrow = c(2,2))
plot(lm.fit)

#non-linear fit again (residual vs fitted) there aren't many major outliers,
#but there are points that have residuals about three times the RSE

#point 14 has an abnormally large leverage

#some interaction terms that are statistically significant
#these were created using the greatest correlation relationships
summary(lm(mpg ~ displacement*cylinders, data = Auto))
summary(lm(mpg ~ horsepower*cylinders, data = Auto))
summary(lm(mpg ~ displacement*weight, data = Auto))
summary(lm(mpg ~ displacement*horsepower, data = Auto))
summary(lm(mpg ~ horsepower*weight, data = Auto))

#other relationships:

summary(lm(mpg ~ displacement, data = Auto))
summary(lm(mpg ~ log(displacement), data = Auto))

summary(lm(mpg ~ displacement, data = Auto))
summary(lm(mpg ~ sqrt(displacement), data = Auto))

summary(lm(mpg ~ weight, data = Auto))
summary(lm(mpg ~ poly(weight,2), data = Auto))

#switch to carseats data (question 10)
attach(Carseats)

lm.fit <- lm(Sales ~ Price + Urban + US)

summary(lm.fit)

?Carseats

# Variables
# Sales: Unit sales (in thousands) at each location
# Price: Price company charges for car seats at each site
# Urban: Urban or rural location
# US: US or non-US location

# Interpretation
# Price: sales are greater when the price is lower (statistically significant)
# UrbanYes: sales are greater for urban locations (non-statistically significant)
# USYes: sales are greater when the store is in the US (statistically significant)
# Intercept: 13k units sold at least regardless of the price, urban, or US (statistically significant)

# equation:
# Sales = b0 + b1*Price + b2*UrbanYes + b3*USYes

# Reject the null hypothesis for Price and USYes

lm.fit <- lm(Sales ~ Price + US)
summary(lm.fit)

# With urban removed, the adj. R-squared increased and the residual standard error decreased
# therefore the model is better

#95% confidence intervals for coefficients
confint(lm.fit)

#diagnostic plots
par(mfrow = c(2,2))
plot(lm.fit)

#no outliers, but there are high leverage observations

#clear environment
rm(list=ls())

#problem 11

set.seed(1)
x <- rnorm(100)
y <- 2*x + rnorm(100)

#linear model, no intercept
lm_no_intercept <- lm(y ~ 0 + x)
summary(lm_no_intercept)

# coefficient: 1.9939
# standard error: 0.1065
# t-statistic: 18.73
# p-value: <2e-16
# the coefficient of X is statistically significant

#linear model, no intercept
lm_no_intercept_2 <- lm(x ~ 0 + y)
summary(lm_no_intercept_2)

# coefficient: 0.39111
# standard error: 0.02089
# t-statistic: 18.73
# p-value: <2e-16
# the coefficient of y is statistically significant

# the orientation of the data changed so the slope and standard error is different,
# but their change does not effect the t-statistic or p-value

#calculating t-statistic numerically
n = length(x)
sum1 = sum(x*y)
sum2 = sum(x^2)
sum3 = sum(y^2)
t_stat = sqrt(n-1)*sum1/sqrt(sum2*sum3 - (sum1^2))

# swtiching x and y in the above equation results in the same equation and
# therefore the same t-statistic

# with an intercept, the t-statistic is the same for regressing x onto y and y onto x
lm_w_intercept <- lm(y ~ x)
summary(lm_w_intercept)

lm_w_intercept_2 <- lm(x ~ y)
summary(lm_w_intercept_2)

rm(list=ls())

#different coefficient extimate without intercept
set.seed(2)
x <- rnorm(100)
y <- 2*x + rnorm(100)

lm_no_intercept <- lm(y ~ 0 + x)
summary(lm_no_intercept)
lm_no_intercept_2 <- lm(x ~ 0 + y)
summary(lm_no_intercept_2)

x <- rnorm(100)
y <- rev(x) #but there are an infinite number of ways to get the same coef est. (ex. y=x)

#same coefficient extimate without intercept
lm_no_intercept <- lm(y ~ 0 + x)
summary(lm_no_intercept)
lm_no_intercept_2 <- lm(x ~ 0 + y)
summary(lm_no_intercept_2)

rm(list=ls())

#different coefficient estimate without intercept
set.seed(1)
x <- rnorm(100, mean = 0, sd = 1)
eps <- rnorm(100, mean = 0, sd=.25^2)
y <- -1 + 0.5*x + eps
length(y)
#y has length of 100, b0 = -1, b1 = 0.5

plot(x,y) #linear function with error added

lm.fit <- lm(y ~ x)
summary(lm.fit) #estimated coef. are about equal to the actual coef.

#plot data and line with legend
plot(x,y, pch=1)
abline(lm.fit, col=2, lty=1)
legend(-2, 0, legend=c("data", "ols"), col=c("black", "red"), lty=c(NA, 1), pch=c(1, NA))

#poly
poly.fit <- lm(y ~ poly(x,2))
summary(poly.fit)
#worse F-statistic, better residual standard error, better R-squared and adj.R-squared
#x^2 term is not statistically significant
#the extra term improves the fit so it may potentially be better at prediction,
#but not better for inference

# less noise

x <- rnorm(100, mean = 0, sd = 1)
eps <- rnorm(100, mean = 0, sd=.1^2)
y <- -1 + 0.5*x + eps

lm_less_noise.fit <- lm(y ~ x)
summary(lm_less_noise.fit) #estimated coef. are about equal to the actual coef.

#plot data and line with legend
plot(x,y, pch=1)
abline(lm_less_noise.fit, col=2, lty=1)
legend(-2, 0, legend=c("data", "ols"), col=c("black", "red"), lty=c(NA, 1), pch=c(1, NA))


#more noise

x <- rnorm(100, mean = 0, sd = 1)
eps <- rnorm(100, mean = 0, sd=2^2)
y <- -1 + 0.5*x + eps

lm_more_noise.fit <- lm(y ~ x)
summary(lm_more_noise.fit) #estimated coef. are about equal to the actual coef.

#plot data and line with legend
plot(x,y, pch=1)
abline(lm_more_noise.fit, col=2, lty=1)
legend(-2, 0, legend=c("data", "ols"), col=c("black", "red"), lty=c(NA, 1), pch=c(1, NA))

#confidence intervals for lm.fit, lm_less_noise.fit, and lm_more_noise.fit
confint(lm.fit)
confint(lm_less_noise.fit)
confint(lm_more_noise.fit)
#confidence interval is narrower with less noise (because more confident in fit)
# and wider with more noise

#collinearity

rm(list=ls())

set.seed(1)
x1 <- runif(100)
x2 <- 0.5*x1 + rnorm(100)/10
y <- 2 + 2 * x1 + 0.3 * x2 + rnorm(100)

cor(x1,x2)
plot(x1,x2)

lm.fit <- lm(y ~ x1 + x2)
summary(lm.fit) #est coef are 2.13, 1.44, and 1.01, real coef are 2,2,1
# can reject null hypothesis that b1 = 0, but not b2 = 0

lm2.fit <- lm(y ~ x1)
summary(lm2.fit)
#can reject the null hypothesis that b1 = 0

lm3.fit <- lm(y ~ x2)
summary(lm3.fit)
#can reject the null hypothesis that b1 = 0

# these do not contradict each other, because collinearity exists between x1 and x2
# the conclusion is that the full model's p-values cannot be trusted to identify
# independent variables that are statistically significant

# add a mismeasured observation

x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y <- c(y, 6)


cor(x1,x2) #lower correlation
plot(x1,x2)

lm.fit <- lm(y ~ x1 + x2)
summary(lm.fit) #est coef are 2.13, 1.44, and 1.01, real coef are 2,2,1
# can reject null hypothesis that b1 = 0, but not b2 = 0

lm2.fit <- lm(y ~ x1)
summary(lm2.fit)
#can reject the null hypothesis that b1 = 0

lm3.fit <- lm(y ~ x2)
summary(lm3.fit)
#can reject the null hypothesis that b1 = 0

# the new data point makes the model better in terms of R-squared, adj. R-squared values
# RSE is worse for x1, and x1 est. coef. is now not statistically significant while
# x2 est. coef. is

#diagnostic plots
par(mfrow = c(2,2))
plot(lm.fit)

#diagnostic plots
par(mfrow = c(2,2))
plot(lm2.fit)

#diagnostic plots
par(mfrow = c(2,2))
plot(lm3.fit)

plot(x1, y)#not a leverage point
plot(x2, y)#leverage point

# the new data point is not a leverage point for x1, but is for x2 and is within the noraml
# range for y so not an outlier

#predicting per capita crime from Boston data set

rm(list=ls())

LoadLibraries()

attach(Boston)
names(Boston)

lm1.fit <- lm(crim ~ zn)
lm2.fit <- lm(crim ~ indus)
lm3.fit <- lm(crim ~ chas)
lm4.fit <- lm(crim ~ nox)
lm5.fit <- lm(crim ~ rm)
lm6.fit <- lm(crim ~ age)
lm7.fit <- lm(crim ~ dis)
lm8.fit <- lm(crim ~ rad)
lm9.fit <- lm(crim ~ tax)
lm10.fit <- lm(crim ~ ptratio)
lm11.fit <- lm(crim ~ black)
lm12.fit <- lm(crim ~ lstat)
lm13.fit <- lm(crim ~ medv)

#all but chas have est. coef. which are statistically significant
summary(lm1.fit)
plot(zn, crim)
abline(lm1.fit, col=2)

summary(lm2.fit)
plot(indus, crim)
abline(lm2.fit, col=2)

summary(lm3.fit)
plot(chas, crim)
abline(lm3.fit, col=2)

summary(lm4.fit)
plot(nox, crim)
abline(lm4.fit, col=2)

summary(lm5.fit)
plot(rm, crim)
abline(lm5.fit, col=2)

summary(lm6.fit)
plot(age, crim)
abline(lm6.fit, col=2)

summary(lm7.fit)
plot(dis, crim)
abline(lm7.fit, col=2)

summary(lm8.fit)
plot(rad, crim)
abline(lm8.fit, col=2)

summary(lm9.fit)
plot(tax, crim)
abline(lm9.fit, col=2)

summary(lm10.fit)
plot(ptratio, crim)
abline(lm10.fit, col=2)

summary(lm11.fit)
plot(black, crim)
abline(lm11.fit, col=2)

summary(lm12.fit)
plot(lstat, crim)
abline(lm12.fit, col=2)

summary(lm13.fit)
plot(medv, crim)
abline(lm13.fit, col=2)

# all predictors
lmfull.fit <- lm(crim ~ ., data=Boston)
summary(lmfull.fit)

#not statistically significant: indus, chas, nox, rm, age, tax, ptratio, lstat

#plot est. coefficients for univariate and multivariate regressions
out <- 0

lmfull.fit$coefficients[2]

for(i in 2:14){
  out[[i-1]] <- lmfull.fit$coefficients[[i]]
}

lm4.fit$coefficients[[2]]

uni <- array(c(lm1.fit$coefficients[[2]],
             lm2.fit$coefficients[[2]],
             lm3.fit$coefficients[[2]],
             lm4.fit$coefficients[[2]],
             lm5.fit$coefficients[[2]],
             lm6.fit$coefficients[[2]],
             lm7.fit$coefficients[[2]],
             lm8.fit$coefficients[[2]],
             lm9.fit$coefficients[[2]],
             lm10.fit$coefficients[[2]],
             lm11.fit$coefficients[[2]],
             lm12.fit$coefficients[[2]],
             lm13.fit$coefficients[[2]]))

plot(uni, out)
identify(uni, out)

# cubic regression

lm1.fit <- lm(crim ~ poly(zn,3))
lm2.fit <- lm(crim ~ poly(indus,3))
lm4.fit <- lm(crim ~ poly(nox,3))
lm5.fit <- lm(crim ~ poly(rm,3))
lm6.fit <- lm(crim ~ poly(age,3))
lm7.fit <- lm(crim ~ poly(dis,3))
lm8.fit <- lm(crim ~ poly(rad,3))
lm9.fit <- lm(crim ~ poly(tax,3))
lm10.fit <- lm(crim ~ poly(ptratio,3))
lm11.fit <- lm(crim ~ poly(black,3))
lm12.fit <- lm(crim ~ poly(lstat,3))
lm13.fit <- lm(crim ~ poly(medv,3))

#all but chas have est. coef. which are statistically significant
summary(lm1.fit) 
summary(lm2.fit)
summary(lm4.fit)
summary(lm5.fit)
summary(lm6.fit)
summary(lm7.fit)
summary(lm8.fit)
summary(lm9.fit)
summary(lm11.fit)
summary(lm12.fit)
summary(lm13.fit)

#all have statistically significant coef for polynomial regression (at least second power) except black


