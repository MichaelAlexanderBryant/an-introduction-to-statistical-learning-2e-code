##3.6.1 Libraries

#install package
install.packages("MASS")

#load libraries
library(MASS)
library(ISLR2)

##3.6.2 Simple Linear Regression

#look at head of Boston data set
head(Boston)

#look at help file for the data set
?Boston

#fit a simple linear regression model (two ways)
#1
lm.fit <- lm(medv ~ lstat, data = Boston)
#2
attach(Boston)
lm.fit <- lm(medv ~ lstat)

#some details about the model
lm.fit

#more detailed summary
summary(lm.fit)

#other information stored in variable
names(lm.fit)

#coefficients
#lm.fit$coefficients or coef(lm.fit)
coef(lm.fit)

#confidence interval
confint(lm.fit)

#confidence interval and prediction
#confidence interval is from the uncertainty of the function
#intervals are centered around the same point but the prediction interval is wider
#prediction interval is from the uncertainty of the function plus irreducible error
predict(lm.fit, data.frame(lstat = (c(5,10,15))),
        interval = "confidence")
predict(lm.fit, data.frame(lstat = (c(5,10,15))),
        interval = "prediction")

#plot points and least squares regression line
plot(lstat, medv)
abline(lm.fit)

#plotting settings abline(a,b) creates line with intercept a and slope b
#some plotting customizations
abline(lm.fit, lwd=3)
abline(lm.fit, lwd=3, col = "red" )
plot(lstat, medv, col="red")
plot(lstat, medv, pch= 20)
plot(lstat, medv, pch = "+")
plot(1:20, 1:20, pch = 1:20)

#diagnostic plots
par(mfrow = c(2,2))
plot(lm.fit)

#alternative method for residuals and studentized residuals
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

#leverage statistics
plot(hatvalues(lm.fit))
#largest index of the largest element of a vector
which.max

##3.6.3 Multiple Linear Regression

#load in data set, because library from package is different
Boston <- read.csv("Boston.csv", na.strings = "?", stringsAsFactors = TRUE)

#regress medv onto lstat and age, print summary
lm.fit <- lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)

#regress mdv onto all predictors, print summary
lm.fit <- lm(medv ~ ., data = Boston)
summary(lm.fit)

#access individual components
#see what's available
?summary.lm
#r-squared
summary(lm.fit)$r.sq
#RSE
summary(lm.fit)$sigma

#calculate VIF (part of car package)
library(car)
vif(lm.fit)

#regression using all variables but one
lm.fit1 <- lm(medv ~ . - age, data = Boston)
summary(lm.fit1)
#alternatively
lm.fit1 <- update(lm.fit, ~. - age)

##3.6.4 Interaction Terms

#regress medv onto lstat, age, and lstat:age
summary(lm(medv ~ lstat * age, data = Boston))

##3.6.5 Non-linear Transformations of the Predictors

#regress medv onto lstat and lstat^2 (p-value suggests an improved model)
lm.fit2 <- lm(medv ~ lstat + I(lstat^2), data = Boston)
summary(lm.fit2)

#use ANOVA to quantify superiority of fit
lm.fit <- lm(medv ~ lstat, data = Boston)
#null hypothesis: two models fit equally well
#large F statistic (135.2) and virtually zero p-value proves superiority
anova(lm.fit, lm.fit2)

#prior residual plot with only lstat showed non-linearity
#lstat^2 residual plot has no non-linearity
par(mfrow = c(2,2))
plot(lm.fit2)

#polynomial predictors can also be created using poly()
#p-values are <0.05 up to fifth order
lm.fit5 <- lm(medv ~ poly(lstat,5), data = Boston)
summary(lm.fit5)

#log transformation
summary(lm(medv ~ log(rm), data = Boston))

##3.6.6 Qualitative Predictors

#remove previous environment data
rm(list=ls())

#head of Carseats data set
#categorical variables automatically have dummies created
head(Carseats)

#regress sales onto all plus income:advertising and price:age interaction terms
lm.fit <- lm(Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)

#automatic dummy variables
attach(Carseats)
contrasts(ShelveLoc)

#learn more
?contrasts

#automatically drops first when creating dummies
#ShelveLocGood has a positive coef and p-value < 0.05 meaning sales is positively correlated to Good compared to bad (first dropped)
#ShelveLocMedium has the same relationship (positive coef, small p-value, bad location results in reduced sales)

##3.6.7 Writing Functions

#create a function to load the libraries for this book
LoadLibraries <- function() {
  library(ISLR2)
  library(MASS)
  print("The libraries have been loaded.")
}

#to examine the function
LoadLibraries

#execute function
LoadLibraries()