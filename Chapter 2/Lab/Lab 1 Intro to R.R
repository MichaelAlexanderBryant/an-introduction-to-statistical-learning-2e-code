##2.3.1: Basic Commands

#create an array using <-
x <- c(1,3,2,5)

#create an array using =
x = c(1,6,2)

#help function for combine/concatenate
?c

#create a second array
y = c(1,4,3)

#length of first array
length(x)

#length of second array
length(y)

#add the two arrays element by element
x + y

#list defined objects (x,y)
ls()

#remove defined objects (x,y)
rm(x,y)

#check list to see no objects
ls()

#remove all objects at once
rm(list = ls())

#help function for matrix
?matrix

#define matrix
x <- matrix(data = c(1,2,3,4), nrow = 2, ncol = 2)

#display matrix
x

#a second way of defining the matrix
x <- matrix(c(1,2,3,4), 2, 2)

#create matrix by rows
matrix(c(1,2,3,4), 2, 2, byrow = TRUE)

#square root every element
sqrt(x)

#square every element
x^2

#create random normal variables for x (mean = 0, sd = 1)
x <- rnorm(50)

#create random normal variables for y (mean = 50, sd = .1) plus x
y <- x + rnorm(50, mean = 50, sd = .1)

#correlation of x and y
cor(x,y)

#reproduce the same exact sets of random numbers
set.seed(1303)
rnorm(50)

#descriptive statistics functions
set.seed(3)
y <- rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

##2.3.2: Graphics

#plotting x vs y
x <- rnorm(100)
y <- rnorm(100)
plot(x,y)

#creating a title and labeling the axes
plot(x,y,xlab="this is the x-axis",
     ylab = "this is the y-axis",
     main = "Plot of X vs Y")

#save file as pdf (could also do jpeg as jpeg())
#plot with green circles
#dev.off indicates that the plot is done
pdf("Figure.pdf")
plot(x, y, col = "green")
dev.off()

#sequence from 1 to 10
x <- seq(1,10)
x <- 1:10

#sequence from -pi to pi in 50 numbers
x <- seq(-pi, pi, length = 50)

#contour plot
y <- x
f <- outer(x, y, function(x, y) cos(y) / (1 + x^2))
contour(x, y, f)

contour(x, y, f, nlevels = 45, add = T)

fa <- (f - t(f)) / 2
contour(x, y, fa, nlevels = 15)

#color-coded plot (heatmap)
image(x, y, fa)

#change perspective
persp(x, y, fa)
persp(x, y, fa, theta = 30)
persp(x, y, fa, theta = 30, phi = 20)
persp(x, y, fa, theta = 30, phi = 70)
persp(x, y, fa, theta = 30, phi = 40)


##2.3.3: Index Data

#create a 4x4 matrix from 1 to 16
A <- matrix(1:16, 4,4)

#different ways of selecting elements
A[2,3]
A[c(1,3), c(2,4)]
A[1:3, 2:4]
A[1:2, ]
A[ , 1:2]
A[1, ]
A[-c(1,3), ]
A[-c(1,3), -c(1,3,4)]

#dimensions of a matrix
dim(A)

##2.3.4: Loading Data

#look at and change working directory
getwd()
setwd("C:/Users/malex/Documents/Introduction to Statistical Learning Labs")

#load data set (specifying "?" as a missing values and strings as categorical)
#view data set
#look at head of data
Auto <- read.csv("Auto.csv", na.strings = "?", stringsAsFactors = TRUE)
View(Auto)
head(Auto)

#dimensions of the data frame
dim(Auto)

#first five rows of the data frame
Auto[1:4, ]

#omit missing observations
Auto <- na.omit(Auto)
dim(Auto)

#column names
names(Auto)


##2.3.5: Additional Graphical and Numerical Summaries

#plot variables
plot(Auto$cylinders, Auto$mpg)

#alternatively, can plot like this
attach(Auto)
plot(cylinders, mpg)

#convert quantitative variable to qualitative
cylinders <- as.factor(cylinders)

#automatically plots qualitative variables as boxplots
plot(cylinders, mpg)

#customize the plots
plot(cylinders, mpg, col="red")
plot(cylinders, mpg, col = "red", varwidth = TRUE)
plot(cylinders, mpg, col = "red", varwidth = TRUE, horizontal = TRUE)
plot(cylinders, mpg, col = "red", varwidth = TRUE, xlab = "cylinders", ylab = "MPG")

#histograms and customizations where col = 2 is the same as col = "red"
hist(mpg)
hist(mpg, col = 2)
hist(mpg, col = 2, breaks = 15)

#scatter plot matrix
pairs(Auto)

#scatter plot matrix with five variables
pairs(~ mpg + displacement + horsepower + weight + acceleration, data = Auto)

#identify a point (click on the point(s) and select finish)
plot(horsepower, mpg)
identify(horsepower, mpg, name)

#descriptive statistics of every variable
summary(Auto)

#descriptive statistics of one variable
summary(mpg)

#save commands
savehistory()

#load commands
loadhistory()

#quit R
q()
