###10.9 Lab: Deep Learning

##10.9.1 A Single Layer Network on the Hitters Data

#set up data, seperate training and test data
library(ISLR2)
Gitters <- na.omit(Hitters)
n <- nrow(Gitters)
set.seed(13)
ntest <- trunc(n/3)
testid <- sample(1:n, ntest)

#linear model for comparison
lfit <- lm(Salary ~ ., data = Gitters[-testid,])
lpred <- predict(lfit, Gitters[testid,])
#with(): first argument is the dataframe and the second calls from it
with(Gitters[testid,], mean(abs(lpred - Salary))) # =254.67 MAE

#lasso model for comparison
#scale standardizes the matrix (mean = 0, variance = 1)
x <- scale(model.matrix(Salary ~ . - 1, data = Gitters)) # - 1 omits intercept
y <- Gitters$Salary
library(glmnet)
cvfit <- cv.glmnet(x[-testid, ], y[-testid], type.measure = "mae")
cpred <- predict(cvfit, x[testid, ], s = "lambda.min")
mean(abs(y[testid] - cpred)) # =252.30 MAE

#set up model structure of neural network
library(keras)
modnn <- keras_model_sequential() %>%
  layer_dense(units = 50, activation = "relu",
              input_shape = ncol(x)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 1)

# %>% is a pipe operator that passes the previous term as the first argument to the next function
# another example: x <- scale(model.matrix(Salary ~ . - 1, data = Gitters))
# can be rewritten as: x <- model.matrix(Salary ~ . - 1, data = Gitters) %>% scale()
# used for readability

# neural network has a single hidden layer with 50 hidden units and ReLU activation function
# then a dropout layer with 40% of the 50 activations from the previous layer are
# set to zero during each iteration of the stochastic gradient descent
# last layer has one unit (output layer)

# goal is to minimize MSE
modnn %>% compile(loss = "mse",
                  optimizer = optimizer_rmsprop(),
                  metrics = list("mean_absolute_error"))

# now set up epochs and batch_size. the latter set to 32 which means for each
# step of SGD, the algorithm randomly selects 32 training observations for computation
# of the gradient. Epoch is the amount of steps needed to process "n" observations.
# training set n = 176 therefore 176/32 = 5.5 SGD steps
# validation_data allows for MAE to be tracked

history <-  modnn %>% fit(
  x[-testid, ], y[-testid], epochs = 1500, batch_size = 32,
  validation_data = list(x[testid, ], y[testid])
)

# val_MAE: 251.75

install.packages('ggplot2')

#plots for loss and MAE for training and validation sets
plot(history)

#running fit again will continue where the previous fit() left off

#evaluate neural network on test set
npred <- predict(modnn, x[testid, ])
mean(abs(y[testid] - npred)) #=256.14 MAE

##10.9.2 A Multilayer Network on the MNIST Digit Data

#load MNIST (from keras)
mnist <- dataset_mnist()
x_train <- mnist$train$x
g_train <- mnist$train$y
x_test <- mnist$test$x
g_test <- mnist$test$y
dim(x_train) # 60,000 images, 28x28 shaped in a 3D array (60000x28x28)
dim(x_test) # 10,000 images, 28x28 shaped in a 3D array (10000x28x28)

#reshape into a matrix and one-hot encode the class label (g)
x_train <- array_reshape(x_train, c(nrow(x_train), 784)) #28*28=784 features
x_test <- array_reshape(x_test, c(nrow(x_test), 784))
y_train <- to_categorical(g_train, 10) #10 categories 0 to 9 digits
y_test <- to_categorical(g_test, 10)

#rescale grayscale values which are between 0 and 255
x_train <- x_train/255
x_test <- x_test/255

#fit neural network
modelnn <- keras_model_sequential()
modelnn %>%
  layer_dense(units = 256, activation = "relu",
              input_shape = c(784)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = "softmax")

#input 784 units (28*28)
#first hidden layer 256 units with ReLU activation at 40% dropout
#second hidden layer at 128 units with ReLU activation at 30% dropout
#output 10 units (0 to 9) with softmax activation (multi-class probabilities)

summary(modelnn)
#parameters include bias term dense_2 param # (784+1)*256 = 200,960
#subscripts have no bearing on the model, if model were initialized again,
#they would change, just indicate different layers

#specify the fitting algorithm
#minimize cross-entropy
modelnn %>% compile(loss = "categorical_crossentropy",
                    optimizer = optimizer_rmsprop(),
                    metrics = c("accuracy"))


#supply training data and fit the model
#training data is split: 80% training, 20% validation
#one epoch = (60000*.8)/128 = 375 gradient (SGD) steps
system.time(
  history <- modelnn %>%
    fit(x_train, y_train, epochs = 30, batch_size = 128,
        validation_split = 0.2)
)

#val_accuracy = 97.83%
plot(history, smooth = FALSE)

#test error function
accuracy <- function(pred, truth) {
  mean(drop(pred) == drop(truth))
}

#calculate test error
modelnn %>% predict_classes(x_test) %>% accuracy(g_test) #= 98.09% test accuracy

#logistic regression using keras (remove hidden layers)
#could use glmnet version of logistic regression, but keras is faster for large data set
modellr <- keras_model_sequential() %>%
  layer_dense(input_shape = 784, units = 10,
              activation = "softmax")
summary(modellr)

modellr %>% compile(loss = "categorical_crossentropy",
                    optimizer = optimizer_rmsprop(),
                    metrics = c("accuracy"))
modellr %>% fit(x_train, y_train, epochs = 30,
                batch_size = 128, validation_split = 0.2)
modellr %>% predict_classes(x_test) %>% accuracy(g_test) #=92.86% test accuracy

##10.9.3 Convolutional Neural Networks

#fit a CNN to the CIFAR100 data set available through keras

cifar100 <- dataset_cifar100()
names(cifar100)
x_train <- cifar100$train$x
g_train <- cifar100$train$y
x_test <- cifar100$test$x
y_test <- cifar100$test$y

dim(x_train) #50000 32 32 3 (50000 images, 32x32, 3 channels for color)
range(x_train[1,,,1])#13 255

#one-hot encode the response to produce 100-column binary matrix
x_train <- x_train/255
x_test <- x_test/255
y_train <- to_categorical(g_train, 100)
dim(y_train) #50000 100

#look at some of the training images
library(jpeg)
par(mar = c(0, 0, 0, 0), mfrow = c(5,5))
index <- sample(seq(50000), 25)
for (i in index) plot(as.raster(x_train[i,,, ])) #as.raster converts the feature map to be plotted as a color image

#initialize structure
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3),
                padding = "same", activation = "relu",
                input-shape = c(32,32,3)) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3,3),
                padding = "same", activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_conv_2d(filters = 128, kernel_size = c(3,3),
                padding = "same", activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_conv_2d(filters = 256, kernel_size = c(3,3),
                padding = "same", activation = "relu") %>%
  layer_max_pooling_2d(pool_size=c(2,2)) %>%
  layer_flatten() %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_dense(units = 100, activation = "softmax")

summary(model)

#padding = "same" ensures that the output channels have the same dimensions as the input channels
#input is 32x32x3
#first layer_conv_2d: 32 channels uses a 3x3 convolution filter
#first layer_max_pooling: 2x2 blocks
#channels halve in both dimensions after each max-pooling operations
#after the last max pooling layer, the output is 256 channels of 2x2 which are flattened to a size of 1024

model %>% compile(loss = "categorical_crossentropy",
                  optimizer = optimizer_rmsprop(), metrics = c("accuracy"))
history <- model %>% fit(x_train, y_train, epochs = 30,
                         batch_size = 128, validation_split = 0.2)
model %>% predict_classes(x_test) %>% accuracy(g_test) #= 45.61% test accuracy

##10.9.4 Using Pretrained CNN Models

#change working directory
getwd()
setwd("C:/Users/malex/Documents/Introduction to Statistical Learning/Chapter 10/Lab")

#import 6 saved animal images from directory
img_dir <- "book_images"
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

#classify the six images
pred6 <- model %>% predict(x) %>%
  imagenet_decode_predictions(top = 3)
names(pred6) <- image_names
print(pred6)


##10.9.5 IMDb Document Classification

#document classification on IMDb data set which is available from keras
#limit dictionary size to 10,000 most frequently-used words and tokens
max_features <- 10000
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

x_train_1h <- one_hot(x_train, 10000)
x_test_1h <- one_hot(x_test, 10000)
dim(x_train_1h) #25000 10000
nnzero(x_train_1h)/(25000*10000) #=1.3% of entries are nonzero
#sparse matrix saves memory by creating a matrix based on which entries are 1
#and removes the zeros which makes up 98.7% of the original one-hot encoded matrix

#create a validation set of size 2000 leaving 23000 for training
set.seed(3)
ival <- sample(seq(along = y_train), 2000)

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
              input_shape = c(10000)) %>%
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
              input_shape = c(10000)) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")
model %>% compile(optimizer = "rmsprop",
                  loss = "binary_crossentropy",
                  metrics = c("accuracy"))
history <- model %>% fit(
  x_train_1h[-ival, ], y_train[-ival], epochs = 20,
  batch_size = 512, validation_data = list(x_test_1h, y_test)
)
#test_accuracy = 84.76%

##10.9.6 Recurrent Neural Networks

#use IMDb data set

max_features <- 10000
imdb <- dataset_imdb(num_words = max_features)
c(c(x_train, y_train), c(x_test, y_test)) %<-% imdb

wc <- sapply(x_train, length)
median(wc) #178
sum(wc <= 500)/length(wc) #0.91568 (over 91% of the documents have fewer than 500 words)

#RNN requires all document sequences to have the same length
#restrict document lengths to the last L=500 words and pad the beginning of shorter ones with blanks
maxlen <- 500
x_train <- pad_sequences(x_train, maxlen=maxlen)
x_test <- pad_sequences(x_test, maxlen=maxlen)
dim(x_train) #25000 500
dim(x_test) #25000 500
x_train[1, 490:500]

#at this point, each of the 500 words in the document are represented as an integer
#corresponding to the location of that word in the 10,000-word dictionary

#initialize recurrent neural network structure
#first layer (embedding) one-hot encodes each document as a 500x10000 matrix
#then maps these 10000 dimensions down to 32
model <- keras_model_sequential() %>%
  layer_embedding(input_dim = 10000, output_dim = 32) %>%
  layer_lstm(units = 32) %>%
  layer_dense(units = 1, activation = "sigmoid")

#fit and compute test accuracy
model %>% compile(optimizer = "rmsprop",
                  loss = "binary_crossentropy",
                  metrics = c("acc"))
history <- model %>% fit(x_train, y_train, epochs = 10,
                         batch_size = 128, validation_data = list(x_test, y_test))
plot(history)
predy <- predict(model, x_test) > 0.5
mean(abs(y_test == as.numeric(predy))) #=85.66% test accuracy

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

#fit linear AR model to training data using lm(), predict test
arfit <- lm(log_volume ~ ., data = arframe[istrain, ])
arpred <- predict(arfit, arframe[!istrain, ])
V0 <- var(arframe[!istrain, "log_volume"])
1 - mean((arpred - arframe[!istrain, "log_volume"])^2) / V0 #R-squared = 0.4132

#refit model including factor day_of_week
arframed <- data.frame(day = NYSE[-(1:5), "day_of_week"], arframe)
arfitd <- lm(log_volume ~ ., data = arframed[istrain, ])
arpredd <- predict(arfitd, arframed[!istrain, ])
1 - mean((arpredd - arframe[!istrain, "log_volume"])^2) / V0 #R-squared = 0.4599

#to fit RNN, reshape data
n <- nrow(arframe)
xrnn <- data.matrix(arframe[, -1]) #extract nx15 matrix of lagged versions of 3 predictor variables
xrnn <- array(xrnn, c(n,3,5)) #convert matrix to nx3x5 array
xrnn <- xrnn[,, 5:1] #reverses order of lagged variables so index 1 is furthest back in time and index 5 is closest
xrnn <- aperm(xrnn, c(1,3,2)) #"partial transpose" to rearrange the coordinates of the array in format RNN expects
dim(xrnn) #6046 5 3

#RNN with 12 hidden units
model <- keras_model_sequential() %>%
  layer_simple_rnn(units = 12,
                   input_shape = list(5,3),
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
1 - mean((kpred - arframe[!istrain, "log_volume"])^2) / V0 #R-squared = 0.3515

#alternative to above structure:
#model <- keras_model_sequential() %>%
#         layer_flatten(input_shape = c(5,3)) %>%
#         layer_dense(units = 1)

#flatten takes the input sequence and turns it into a long vector of predictors
#resulting in a linear AR model, to fit a nonlinear AR model, add a hidden layer

#since already have lagged variables in a matrix from AR model earlier, can use
#lm() to fit nonlinear AR model without flattening

#extract model matrix x from arframed which includes day_of_week variable
x <- model.matrix(log_volume ~ . - 1, data = arframed) #-1 avoids creation of column of ones for intercept
colnames(x)          
#day_of_week is a five level factor for the five trading days per week, -1 results in five rather than four dummy variables

#fit nonlinear AR model
arnnd <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = "relu",
              input_shape = ncol(x)) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1)
arnnd %>% compile(loss = "mse",
                  optimizer = optimizer_rmsprop())
history <- arnnd %>% fit(
  x[istrain, ], arframe[istrain, "log_volume"], epochs = 100,
  batch_size = 32, validation_data = list(x[!istrain, ], arframe[!istrain, "log_volume"])
)
plot(history)
npred <- predict(arnnd, x[!istrain, ])
1 - mean((arframe[!istrain, "log_volume"] - npred)^2) / V0 #R-squared = 0.4233