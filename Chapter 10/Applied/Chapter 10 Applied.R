#gradient descent
#function R = sin(b) + b/10
#learning rate = 0.1
#starting point: b = 2.3

b <- seq(-6,6,.1)
R <- sin(b) + b/10
plot(x=b,y=R)

rho = 0.1
b_gd <- rep(2.3, 101)

for (i in 1:100){
  gd <- rho*(cos(b_gd[i])+(1/10))
  b_gd[i+1] <- b_gd[i] - gd
  
}

#plot gradient descent
plot(x=seq(1,101,1), y=b_gd)
which.max(b_gd)
b_gd[101] #=4.612

#repeat, but initialized at b=1.4

rho = 0.1
b_gd <- rep(1.4, 101)

for (i in 1:100){
  gd <- rho*(cos(b_gd[i])+(1/10))
  b_gd[i+1] <- b_gd[i] - gd
  
}

#plot gradient descent
plot(x=seq(1,101,1), y=b_gd)
which.min(b_gd)
b_gd[101] #=-1.67

#fit a neural network to the Default data set
#use a single hidden layer with 10 units and dropout regularization
#compare classification performance with (linear) logistic regression
library(ISLR2)
library(keras)

#logistic regression
View(Default)
sum(is.na(Default))

x <- scale(model.matrix(default ~ . - 1, data = Default))
y <- Default$default

set.seed(2)
train <- sample(1:nrow(x), nrow(x)*.8)
default.test <- y[-train]
default.train <- y[train]

library(glmnet)
cvfit <- cv.glmnet(x[train, ], as.numeric(default.train), family = "binomial", type.measure = "class")
cprob <- predict(cvfit, x[-train, ], s = "lambda.min", type = "response")
lr.pred <- rep("No", length(cprob))
lr.pred[cprob >0.5] <- "Yes"
table(lr.pred, default.test)
1-(1927+23)/(43+7+1927+23) #2.5% error rate



model_nn <-  keras_model_sequential() %>%
  layer_dense(10, activation = 'relu',
              input_shape = ncol(x[train, ])) %>%
  layer_dropout(rate = 0.7) %>%
  layer_dense(units = 1, activation = 'sigmoid')

model_nn %>% compile(loss = "categorical_crossentropy",
                  optimizer = optimizer_rmsprop(),
                  metrics = "accuracy")

history <- model_nn %>% fit(x[train, ], as.numeric(as.factor(default.train)), epochs = 100, batch_size = 254,
                            validation_data = list(x[-train, ], as.numeric(as.factor(default.test))))

accuracy <- function(pred, truth){
  mean(drop(pred) == drop(truth))
}
1 - model_nn %>% predict_classes(x[-train, ]) %>% accuracy(as.numeric(as.factor(default.test))) # 3.1% error rate

#classify animals from personal photographs using imagenet

#change working directory
getwd()
setwd("C:/Users/malex/Documents/Introduction to Statistical Learning/Chapter 10/Applied")

#import 10 saved animal images from directory
img_dir <- "animal_images"
image_names <- list.files(img_dir)
num_images <- length(image_names)
x <- array(dim = c(num_images, 224, 224, 3))
for (i in 1:num_images){
  img_path <- paste(img_dir, image_names[i], sep = "/")
  img <- image_load(img_path, target_size = c(224, 224))
  x[i,,, ] <- image_to_array(img)
}
#preprocess images
x <- imagenet_preprocess_input(x)

#load trained network
model <- application_resnet50(weights = "imagenet")
summary(model)

#classify the ten images and display the top 5 results
pred6 <- model %>% predict(x) %>%
  imagenet_decode_predictions(top = 5)
names(pred6) <- image_names
print(pred6)

#fit a lag-5 autoregressive model to the NYSE data
#refit the model with 12-level factor representing the month
#does this factor improve the performance





#time series prediction with NYSE data set
library(ISLR2)
xdata <- data.matrix(
  NYSE[, c("DJ_return", "log_volume", "log_volatility")]
)
istrain <- NYSE[, "train"] #contains TRUE for each year that is in the training set, FALSE for test set

#create function to lag variables 
lagm <- function(x, k = 1){
  n <- nrow(x)
  pad <- matrix(NA, k, ncol(x))
  rbind(pad, x[1:(n-k), ])
}

#create the lags
arframe <- data.frame(log_volume = xdata[, "log_volume"],
                      L1 = lagm(xdata, 1), L2 = lagm(xdata,2),
                      L3 = lagm(xdata, 3), L4 = lagm(xdata,4),
                      L5 = lagm(xdata,5))

#missing values in first five rows (due to lag), so remove these rows
arframe <- arframe[-(1:5), ]
istrain <- istrain[-(1:5)]

#month factor
install.packages('lubridate')
library(lubridate)
arframed <- data.frame(month = month(ymd(NYSE[-(1:5),"date"])), arframe)

install.packages("devtools")
devtools::install_github("ben519/mltools")
library(data.table)
library(mltools)
newdata <- one_hot(as.data.table(arframed$month))
arframed <- subset(arframed, select=-c(month))
arframed <- cbind(newdata, arframed)

arfitd <- lm(log_volume ~ ., data = arframed[istrain, ])
arpredd <- predict(arfitd, arframed[!istrain, ])
V0 <- var(arframe[!istrain, "log_volume"])
1 - mean((arpredd - arframe[!istrain, "log_volume"])^2) / V0 #R-squared = 0.4170 better score than without month factor

#to fit RNN, reshape data
n <- nrow(arframe)
xrnn <- data.matrix(arframe[, -1]) #extract nx15 matrix of lagged versions of 3 predictor variables
xrnn <- array(xrnn, c(n,3,5)) #convert matrix to nx3x5 array
xrnn <- xrnn[,, 5:1] #reverses order of lagged variables so index 1 is furthest back in time and index 5 is closest
xrnn <- aperm(xrnn, c(1,3,2)) #"partial transpose" to rearrange the coordinates of the array in format RNN expects
dim(xrnn) #6046 5 3

#linear AR model
model <- keras_model_sequential() %>%
        layer_flatten(input_shape = c(5,3)) %>%
        layer_dense(units = 1)
model %>% compile(optimizer = optimizer_rmsprop(),
                  loss = "mse")

#two forms of dropout, first for the input sequence feeding into the layer
#second for the previous hidden layer feeding into the layer
#one unit output for response

#fit and compute R-squared on test
history <- model %>% fit(
  xrnn[istrain,, ], arframe[istrain, "log_volume"],
  batch_size = 64, epochs = 200,
  validation_data = list(xrnn[!istrain,, ], arframe[!istrain, "log_volume"])
)
kpred <- predict(model, xrnn[!istrain,, ])
1 - mean((kpred - arframe[!istrain, "log_volume"])^2) / V0 #R-squared = 0.3678

#linear AR from lab using lm(): R-squared = 0.4132 (slightly better score)

#nonlinear AR model
model <- keras_model_sequential() %>%
  layer_flatten(input_shape = c(5,3)) %>%
  layer_dense(units=200, activation = 'relu') %>%
  layer_dense(units = 1)
model %>% compile(optimizer = optimizer_rmsprop(),
                  loss = "mse")

#two forms of dropout, first for the input sequence feeding into the layer
#second for the previous hidden layer feeding into the layer
#one unit output for response

#fit and compute R-squared on test
history <- model %>% fit(
  xrnn[istrain,, ], arframe[istrain, "log_volume"],
  batch_size = 64, epochs = 200,
  validation_data = list(xrnn[!istrain,, ], arframe[!istrain, "log_volume"])
)
kpred <- predict(model, xrnn[!istrain,, ])
1 - mean((kpred - arframe[!istrain, "log_volume"])^2) / V0 #R-squared = 0.3753


##left off at problem 12


#time series prediction with NYSE data set
library(ISLR2)

xdata <- data.matrix(
  NYSE[, c("day_of_week","DJ_return", "log_volume", "log_volatility")]
)
istrain <- NYSE[, "train"] #contains TRUE for each year that is in the training set, FALSE for test set

#create function to lag variables 
lagm <- function(x, k = 1){
  n <- nrow(x)
  pad <- matrix(NA, k, ncol(x))
  rbind(pad, x[1:(n-k), ])
}

#create the lags
arframe <- data.frame(log_volume = xdata[, "log_volume"],
                      L1 = lagm(xdata, 1), L2 = lagm(xdata,2),
                      L3 = lagm(xdata, 3), L4 = lagm(xdata,4),
                      L5 = lagm(xdata,5))

#missing values in first five rows (due to lag), so remove these rows
arframe <- arframe[-(1:5), ]
istrain <- istrain[-(1:5)]


#to fit RNN, reshape data
n <- nrow(arframe)
xrnn <- data.matrix(arframe[, -1]) #extract nx15 matrix of lagged versions of 3 predictor variables
xrnn <- array(xrnn, c(n,4,5)) #convert matrix to nx3x5 array
xrnn <- xrnn[,, 5:1] #reverses order of lagged variables so index 1 is furthest back in time and index 5 is closest
xrnn <- aperm(xrnn, c(1,3,2)) #"partial transpose" to rearrange the coordinates of the array in format RNN expects
dim(xrnn) #6046 5 4

library(keras)

#RNN with 12 hidden units
model <- keras_model_sequential() %>%
  layer_simple_rnn(units = 12,
                   input_shape = list(5,4),
                   dropout = 0.1, recurrent_dropout = 0.1) %>%
  layer_dense(units = 1)
model %>% compile(optimizer = optimizer_rmsprop(),
                  loss = "mse")

#two forms of dropout, first for the input sequence feeding into the layer
#second for the previous hidden layer feeding into the layer
#one unit output for response

#fit and compute R-squared on test
history <- model %>% fit(
  xrnn[istrain,, ], arframe[istrain, "log_volume"],
  batch_size = 64, epochs = 200,
  validation_data = list(xrnn[!istrain,, ], arframe[!istrain, "log_volume"])
)
kpred <- predict(model, xrnn[!istrain,, ])
1 - mean((kpred - arframe[!istrain, "log_volume"])^2) / V0 #R-squared = 0.4085



#repeat the analysis in lab 10.9.5 on IMDb data set, but vary the dictionary size
#dict. size = 1000, 3000, 5000, 10000

#document classification on IMDb data set which is available from keras
#limit dictionary size to 10,000 most frequently-used words and tokens
max_features <- 5000
imdb <- dataset_imdb(num_words = max_features)
c(c(x_train, y_train), c(x_test, y_test)) %<-% imdb #shortcut for unpacking the list of lists

#each element of x_train is a vector of numbers between 0 and 9999 referring to the number
#of words found in the dictionary

#example: first training document, indicies of first 12 words
x_train[[1]][1:12]

#to see the words, create a function to interface to the dictionary
word_index <- dataset_imdb_word_index()
decode_review <- function(text, word_index){
  word <- names(word_index)
  idx <- unlist(word_index, use.names = FALSE)
  word <- c("<PAD>", "<START>", "<UNK>", "<UNUSED>", word)
  idx <- c(0:3, idx + 3)
  words <- word[match(text, idx, 2)]
  paste(words, collapse = " ")
}
decode_review(x_train[[1]][1:12], word_index)

#write a function to one-hot encode each document in a list of documents
#return a binary matrix in sparse-matrix format
library(Matrix)
one_hot <- function(sequences, dimension){
  seqlen <- sapply(sequences, length)
  n <- length(seqlen)
  rowind <- rep(1:n, seqlen)
  colind <- unlist(sequences)
  sparseMatrix(i = rowind, j= colind,
               dims = c(n,dimension))
}

x_train_1h <- one_hot(x_train, 5000)
x_test_1h <- one_hot(x_test, 5000)
dim(x_train_1h) #25000 10000
nnzero(x_train_1h)/(25000*5000) #=9.4% of entries are nonzero
#sparse matrix saves memory by creating a matrix based on which entries are 1
#and removes the zeros which makes up 98.7% of the original one-hot encoded matrix

#create a validation set of size 2000 leaving 23000 for training
set.seed(3)
ival <- sample(seq(along = y_train), 2000)

accuracy <- function(pred, truth) {
  mean(drop(pred) == drop(truth))
}

#use lasso logistic regression
library(glmnet)
fitlm<- glmnet(x_train_1h[-ival, ], y_train[-ival],
               family = "binomial", standardize = FALSE)
classlmv <- predict(fitlm, x_train_1h[ival, ]) > 0
acclmv <- apply(classlmv, 2, accuracy, y_train[ival] > 0)

#plot accuracy against -log(lambda)
par(mar = c(4,4,4,4), mfrow = c(1,1))
plot(-log(fitlm$lambda), acclmv)


#use a fully-connected neural network with two hidden layers with 16 units and ReLU activation
model <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu",
              input_shape = c(5000)) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")
model %>% compile(optimizer = "rmsprop",
                  loss = "binary_crossentropy",
                  metrics = c("accuracy"))
history <- model %>% fit(x_train_1h[-ival, ], y_train[-ival],
                         epochs = 20, batch_size = 512,
                         validation_data = list(x_train_1h[ival, ], y_train[ival]))

#compute test accuracy
model <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu",
              input_shape = c(5000)) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")
model %>% compile(optimizer = "rmsprop",
                  loss = "binary_crossentropy",
                  metrics = c("accuracy"))
history <- model %>% fit(
  x_train_1h[-ival, ], y_train[-ival], epochs = 20,
  batch_size = 512, validation_data = list(x_test_1h, y_test)
)

#test_accuracy of 1000 dict size = 84.86%
#test_accuracy of 3000 dict size = 86.23% <- best dict. size
#test_accuracy of 5000 dict size = 85.56%
#test_accuracy of 10000 dict size = 84.76%