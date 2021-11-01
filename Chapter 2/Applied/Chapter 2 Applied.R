#change to the correct working directory
getwd()
setwd("C:/Users/malex/Documents/Introduction to Statistical Learning Labs/Chapter 2/Applied")

##college data set

#read the data into R with strings as categorical variables
college <- read.csv("college.csv", stringsAsFactors = TRUE)

#view the college data set
View(college)

#check for na values
any(is.na(college))

#set row names to the name of the college (located in the first column)
rownames(college) <- college[ , 1]
View(college)

#remove the first column containing the university names
college <- college[ , -1]
View(college)

#numerical summary of the data
summary(college)

#scatter matrix of the first ten variables
pairs(college[,1:10])

#plot for out of state tuition
attach(college)
plot(Private, Outstate, xlab = "Private", ylab = "Out of State Tuition")

#create "elite" variable for whether a college contains >50% of students who were in the top 10 percent of their high school 
Elite <- rep("No", nrow(college))
Elite[college$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)

#summary of new variable
summary(college$Elite)
college <- data.frame(college, Elite)

#plot outstate versus elite
attach(college)
plot(Elite, Outstate, xlab = "Elite", ylab="Out of State Tution")

View(college)

#different bin sizes for four variables on a 2x2 grid
par(mfrow = c(2,2))
hist(college$Apps, col = 3, breaks = 40, xlab = "Apps")
hist(college$Accept, col = 4, breaks = 40, xlab = "Accept")
hist(college$Enroll, col = 5, breaks = 40, xlab = "Enroll")
hist(college$F.Undergrad, col = 2, breaks = 40, xlab = "Undergrad")

rm(list = ls())

##auto data set

#read in auto data set, assigning "?" as na, and setting strings as cateogrical
Auto <- read.csv("Auto.csv", na.strings = "?", stringsAsFactors = TRUE)

#omit na values from Auto data set
Auto <- na.omit(Auto)

#check which variables are qualitative and quantitative
summary(Auto)

#range of quantitative variables
range(Auto$mpg)
range(Auto$cylinders)
range(Auto$displacement)
range(Auto$horsepower)
range(Auto$weight)
range(Auto$acceleration)
range(Auto$year)
range(Auto$origin)

#mean of quantitative variables
mean(Auto$mpg)
mean(Auto$cylinders)
mean(Auto$displacement)
mean(Auto$horsepower)
mean(Auto$weight)
mean(Auto$acceleration)
mean(Auto$year)
mean(Auto$origin)

#standard deviation of quantiative variables
sd(Auto$mpg)
sd(Auto$cylinders)
sd(Auto$displacement)
sd(Auto$horsepower)
sd(Auto$weight)
sd(Auto$acceleration)
sd(Auto$year)
sd(Auto$origin)

#remove 10th through 85th observation
Auto <- Auto[-c(10:84), ]

#range of quantitative variables with removed observations
range(Auto$mpg)
range(Auto$cylinders)
range(Auto$displacement)
range(Auto$horsepower)
range(Auto$weight)
range(Auto$acceleration)
range(Auto$year)
range(Auto$origin)

#mean of quantitative variables with removed observations
mean(Auto$mpg)
mean(Auto$cylinders)
mean(Auto$displacement)
mean(Auto$horsepower)
mean(Auto$weight)
mean(Auto$acceleration)
mean(Auto$year)
mean(Auto$origin)

#standard deviation of quantitative variables with removed observations
sd(Auto$mpg)
sd(Auto$cylinders)
sd(Auto$displacement)
sd(Auto$horsepower)
sd(Auto$weight)
sd(Auto$acceleration)
sd(Auto$year)
sd(Auto$origin)

#reload data
Auto <- read.csv("Auto.csv", na.strings = "?", stringsAsFactors = TRUE)

#omit na values again
Auto <- na.omit(Auto)

#scatterplot matrix
pairs(Auto, col = 2)

##boston data set

rm(list=ls())

#install packages for ISLR2
install.packages("ISLR2")

#load data set
library(ISLR2)

#read about the data set
?Boston

#pairwise scatterplots
pairs(Boston, col = 2)

#looking for high values
range(crim)
range(tax)
range(ptratio)

#change type to categorical
Boston$chas <- as.factor(chas)

#count categorical
summary(chas)

#median ptraio
median(ptratio)

#min value
min(medv)

#records with minimum medv
Boston[medv == 5, ]

#summary of Boston data set
summary(Boston)

#histogram of average rooms per dwelling on 1x1 grid
par(mfrow = c(1,1))
hist(rm)

#number of tracts with average number of rooms greater than seven
nrow(Boston[rm > 7, ])

#number of tracts with average number of rooms greater than 8
nrow(Boston[rm > 8, ])

#tracts with average number of rooms greater than 8
Boston[rm > 8, ]
