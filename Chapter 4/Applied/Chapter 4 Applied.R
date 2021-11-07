library(ISLR2)

#look at the data set
View(Weekly)
summary(Weekly)
dim(Weekly)
pairs(Weekly, col=2)

#logistic regression
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
               data = Weekly,
               family = binomial)
summary(glm.fit) #Lag2 is statistically significant

glm.prob <- predict(glm.fit, type="response")

glm.pred <- rep("Down", 1089)
glm.pred[glm.prob > .5] = "Up"

attach(Weekly)
table(glm.pred, Direction)
557/(557+430)
#56.43% precision (true positive rate)
557/(557+48)
#92.07% sensitivity/recall
54/(430+54)
#11.16% specificity

#data 1990 to 2008
train <- (Year < 2009)
test <- Weekly[!train,]
test.target <- Direction[!train]

#logistic regression with hold-out set
lr.fit <- glm(Direction ~ Lag2,
              data = Weekly,
              family = binomial,
              subset = train)
lr.prob <- predict(lr.fit,
                   test,
                   type = "response")
lr.pred <- rep("Down", 104)
lr.pred[lr.prob > .5] <- "Up"

table(lr.pred, test.target)
mean(lr.pred == test.target) #62.5% accuracy

#LDA with hold-out set

library(MASS)

lda.fit <- lda(Direction ~ Lag2,
              data = Weekly,
              subset = train)
lda.pred <- predict(lda.fit,
                   test,
                   type = "response")

lda.class <- lda.pred$class

table(lda.class, test.target)
mean(lda.class == test.target) #62.5% accuracy

#QDA with hold-out set

qda.fit <- qda(Direction ~ Lag2,
               data = Weekly,
               subset = train)
qda.pred <- predict(qda.fit,
                    test,
                    type = "response")

qda.class <- qda.pred$class

table(qda.class, test.target)
mean(qda.class == test.target) #58.65% accuracy

#naive Bayes with hold-out set

library(e1071)

nb.fit <- naiveBayes(Direction ~ Lag2,
                     data = Weekly,
                     subset = train)
nb.fit

nb.class <- predict(nb.fit, test)
table(nb.class, test.target)

mean(nb.class == test.target) #=58.65%

#knn with hold-out set

library(class)

#create training and test matrices and train-target array
training_set <- Lag2[train]
testing_set <- Lag2[!train]
training_targets <- Direction[train]

length(training_set) == length(testing_set)

set.seed(1)

#use k = 1
knn.pred <- knn(train = data.frame(training_set), test = data.frame(testing_set), cl = training_targets, k=1)
table(knn.pred, test.target)
#accuracy
(21+31)/104 #=50.0%

#the models with the best accuracy are Logistic Regression and LDA (62.5%)

#now that the baselines have been established, now experiment with predictors (include others, transformations, interactions) and k for knn

#logistic experiment

cor(Weekly[1:8])

lr.fit <- glm(Direction ~ Lag2 + poly(Year,3),
              data = Weekly,
              family = binomial,
              subset = train)
lr.prob <- predict(lr.fit,
                   test,
                   type = "response")
lr.pred <- rep("Down", 104)
lr.pred[lr.prob > .5] <- "Up"


table(lr.pred, test.target)
mean(lr.pred == test.target) #63.5% accuracy with Lag2 and poly(Year,3), 62.5% only Lag2

#LDA experiment

lda.fit <- lda(Direction ~ Lag2 + poly(Year,3),
               data = Weekly,
               subset = train)
lda.pred <- predict(lda.fit,
                    test,
                    type = "response")

lda.class <- lda.pred$class

table(lda.class, test.target)
mean(lda.class == test.target) #63.5% accuracy with Lag2 and poly(Year,3), 62.5% only Lag2

#QDA experiment
names(Weekly)

qda.fit <- qda(Direction ~ poly(Lag2,2),
               data = Weekly,
               subset = train)
qda.pred <- predict(qda.fit,
                    test,
                    type = "response")

qda.class <- qda.pred$class

table(qda.class, test.target)
mean(qda.class == test.target) #62.5% accuracy with poly(Lag2, 2), 58.65% only Lag2

#naive Bayes experiment

nb.fit <- naiveBayes(Direction ~ Lag2,
                     data = Weekly,
                     subset = train)
nb.fit

nb.class <- predict(nb.fit, test)
table(nb.class, test.target)

mean(nb.class == test.target) #58.65% only Lag2 (couldn't improve accuracy)


#knn experiment

#create training and test matrices and train-target array
training_set <- cbind(poly(Year,3), Lag2)[train,]
testing_set <- cbind(poly(Year,3),Lag2)[!train,]
training_targets <- Direction[train]

length(training_set) == length(testing_set)

set.seed(1)

#use k = 1
knn.pred <- knn(train = training_set, test = testing_set, cl = training_targets, k=2)
table(knn.pred, test.target)
#accuracy
(21+31)/104 #50% with everything I tried, does not change


rm(list=ls())

#predict high or low gas mileage in Auto data set

View(Auto)
attach(Auto)

median(mpg)
length(mpg)

#create target variable where 1 if above median mpg and 0 if below
mpg01 <- rep(0, 392)
mpg01[mpg > median(mpg)] = 1
mod_df <- Auto
mod_df$mpg01 <- mpg01
#remove original mpg variable
mod_df <- subset(mod_df, select = -c(mpg))

View(mod_df)

#looks like cylinders, weight, displacement, and horsepower (in that order) are most highly (neg) correlated with mpg01
cor(mod_df[, names(mod_df) != "name"])
pairs(mod_df[, names(mod_df) != "name"], col=2)

#train-test split 80/20
train <- (1:(392*.8))
test <- mod_df[(392*.8):392,]
test.target <- mpg01[(392*.8):392]

#LDA

#use variables that seem most associated with mpg01
lda.fit <- lda(mpg01 ~ cylinders + weight + displacement + horsepower,
               data = mod_df,
               subset = train)
lda.pred <- predict(lda.fit,
                    test,
                    type = "response")

lda.class <- lda.pred$class

table(lda.class, test.target)
1 - mean(lda.class == test.target) #13.9% error rate

#QDA

#use variables that seem most associated with mpg01
qda.fit <- qda(mpg01 ~ cylinders + weight + displacement + horsepower,
               data = mod_df,
               subset = train)
qda.pred <- predict(qda.fit,
                    test,
                    type = "response")

qda.class <- qda.pred$class

table(qda.class, test.target)
1 - mean(qda.class == test.target) #13.9% error rate


#naive Bayes

nb.fit <- naiveBayes(mpg01 ~ cylinders + weight + displacement + horsepower,
                     data = mod_df,
                     subset = train)
nb.fit

nb.class <- predict(nb.fit, test)
table(nb.class, test.target)

1- mean(nb.class == test.target) #10.1% error rate

#knn

attach(mod_df)

#create training and test matrices and train-target array
training_set <- cbind(cylinders, weight, displacement, horsepower)[(1:(392*.8)),]
testing_set <- cbind(cylinders, weight, displacement, horsepower)[(392*.8):392,]
training_targets <- mpg01[train]

set.seed(1)

knn.pred <- knn(train = training_set, test = testing_set, cl = training_targets, k=4)
table(knn.pred, test.target)

1 - (5 + 57)/79 #21.5% error rate k=1
1 - (5 + 55)/79 #24.1% error rate k=2
1 - (5 + 54)/79 #25.3% error rate k=3
1 - (5 + 53)/79 #26.6% error rate k=4


#writing functions

#create a function that computes and prints 2^3
Power <- function(x) {
  print(x^3)
}
Power(2)

#create a function that computes and prints x^a
Power2 <- function(x,a) {
  print(x^a)
}
Power2(3,8)
Power2(10,3)
Power2(8,17)
Power2(131,3)

#create a function that computes x^a and returns it as a variable
Power3 <- function(x,a) {
  result = x^a
  return(result)
}
Power3(3,8)

#plot using Power3
x <- 1:10
plot(y= Power3(x,2), x = x)
#use log scales
plot(y= Power3(x,2), x = x, log = "x")
plot(y= Power3(x,2), x = x, log = "y")
plot(y= Power3(x,2), x = x, log = "xy")

#create a function that plots Power3
PlotPower <- function(x,a){
  plot(y = Power3(x,a), x = x)
}
PlotPower(1:10, 3)
PlotPower(1:20, 5)

#Boston data set: create a binary variable for being each tract below or above the median crime rate
#use logistic regression, LDA, naive Bayes, and KNN to predict using various subsets of predictors

View(Boston)
attach(Boston)

crim01 <- rep(0, length(crim))
crim01[crim > median(crim)] = 1
mod_df <- Boston
mod_df$crim01 <- crim01
#remove original mpg variable
mod_df <- subset(mod_df, select = -c(crim))

View(mod_df)

#variables with high correlation with crim01: nox, chas, age, dis, rad, tax, indus 
cor(mod_df)
pairs(mod_df)
length(mod_df[,1])

#train-test split 80/20
train <- (1:(506*.8))
test <- mod_df[(506*.8):506,]
test.target <- crim01[(506*.8):506]

attach(mod_df)


#logistic regression
lr.fit <- glm(crim01 ~ nox + chas + age + dis + rad + tax + indus,
              data = mod_df,
              family = binomial,
              subset = train)
lr.prob <- predict(lr.fit,
                   test,
                   type = "response")
lr.pred <- rep(0, 102)
lr.pred[lr.prob > .5] <- 1

table(lr.pred, test.target)
mean(lr.pred == test.target) #94.1% accuracy


#LDA
lda.fit <- lda(crim01 ~ nox + chas + age + dis + rad + tax + indus,
               data = mod_df,
               subset = train)
lda.pred <- predict(lda.fit,
                    test,
                    type = "response")

lda.class <- lda.pred$class

table(lda.class, test.target)
mean(lda.class == test.target) #87.3% accuracy

#naive Bayes
nb.fit <- naiveBayes(crim01 ~ nox + chas + age + dis + rad + tax + indus,
                     data = mod_df,
                     subset = train)
nb.fit

nb.class <- predict(nb.fit, test)
table(nb.class, test.target)

mean(nb.class == test.target) #86.3% accuracy

#knn
training_set <- cbind.data.frame(nox, chas, age, dis, rad, tax, indus)[(1:(506*.8)),]
testing_set <- cbind.data.frame(nox, chas, age, dis, rad, tax, indus)[(506*.8):506,]
training_targets <- crim01[1:(506*.8)]

set.seed(1)

knn.pred <- knn(train = training_set, test = testing_set, cl = training_targets, k=12)
table(knn.pred, test.target)

(8 + 85)/102 #91.2% accuracy k=1
(8 + 85)/102 #91.2% accuracy k=2
(9 + 85)/102 #92.2% accuracy k=3
(9 + 85)/102 #92.2% accuracy k=4,8,10
(7 + 85)/102 #90.2% accuracy k=12
