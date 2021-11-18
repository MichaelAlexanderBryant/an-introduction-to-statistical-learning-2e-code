###12.5 Lab: Unsupervised Learning

##12.5.1 Principal Components Analysis

#view entire data set
View(USArrests)

#view row names (which are the state names)
states <- row.names(USArrests)
states

##view column names
names(USArrests)

#calculate mean of each column (apply mean function to USArrests columns)
#2 indicates columns
apply(USArrests, 2, mean)

#calculate variances of each column
apply(USArrests, 2, var)

#large differences in variances indicates that the variables need to be scaled
#else the principal components would be driven by the variable with the most
#variance (i.e., Assault)

#principal components analysis (automatically centers variable to have mean of zero)
#scale = TRUE scales the variables to have a standard deviation of one
pr.out <- prcomp(USArrests, scale = TRUE)

#view PCA options
names(pr.out)

#can view the original mean and std. deviations of the variables prior to scaling
pr.out$center
pr.out$scale
#principal component loadings
pr.out$rotation
#(the four) principal component score vectors are contained in x
dim(pr.out$x)

#plot first two principal components
#scale = 0 ensures that the arrows are scaled to represent the loadings
biplot(pr.out, scale = 0)

#standard deviations of each principal component
pr.out$sdev
#variance of each principal component
pr.var <- pr.out$sdev^2
pr.var

#proportion of variance explained per principal component
pve <- pr.var / sum(pr.var)
pve #PC1: 62.0%, PC2: 24.7%, PC3: 8.9%, PC4: 4.3%

#plot PVE and cumulative PVE
par(mfrow = c(1,2))
plot(pve, xlab="Principal Component",
     ylab="Proportion of Variance Explained", ylim = c(0,1),
     type = "b")
plot(cumsum(pve), xlab = "Principal Component", 
     ylab="Cumulative Proportion of Variance Explained",
     ylim = c(0,1), type = "b")

#where cumsum computes cumulative sums, example:
a <- c(1,2,8,-3)
cumsum(a)

##12.5.2 Matrix Completion

#convert dataframe into matrix
X <- data.matrix(scale(USArrests))
pcob <- prcomp(X)
summary(pcob)

#singular value decomposition
sX <- svd(X)
names(sX) #u,d,v where v contains the loading matrix
round(sX$v, 3)
pcob$rotation
#u is the matrix of standardized scores, d is the standard deviations
#recover the score vectors
t(sX$d * t(sX$u))
pcob$x

#to illustrate using PCA for matrix completion
#omit 20 entries of the 50x2 data matrix at random
nomit <- 20
set.seed(15)
ina <-sample(seq(50), nomit) #20 integers from 1 to 50 for selecting states for NA
inb <- sample(1:4, nomit, replace = TRUE) #20 integers from 1 to 4 for selecting features for NA
Xna <- X
index.na <- cbind(ina, inb) #two column matrix with columns ina and inb
Xna[index.na] <- NA #assign NA values

#use svd for illustration, but could use prcomp
#write a function that takes a matrix and returns an approximation to the matrix using svd
fit.svd <-  function(X, M=1){
  svdob <- svd(X)
  with(svdob,
       u[, 1:M, drop = FALSE] %*% (d[1:M] * t(v[, 1:M, drop = FALSE])))
  #alternative to with(...)
  #svdob$u[, 1:M, drop = FALSE] %*%
  # (avdob$d[1:M]*t(avdob$v[, 1:M, drop  = FALSE]))
}

#initialize X-hat by replacing the missing values with the column means of non-missing entries
Xhat <- Xna
xbar <- colMeans(Xna, na.rm = TRUE)
Xhat[index.na] <- xbar[inb]

#for measuring progress of iterations
thresh <- 1e-7
rel_err <- 1
iter <- 0
ismiss <- is.na(Xna) #logical matrix indicating na in Xna
mssold <- mean((scale(Xna, xbar, FALSE)[!ismiss])^2)
mss0 <- mean(Xna[!ismiss]^2) #mean squared error of the non-missing elements of old Xhat in mssold

while(rel_err > thresh){
  iter <- iter + 1
  # Step 2(a)
  Xapp <- fit.svd(Xhat, M=1)
  # Step 2(b)
  Xhat[ismiss] <- Xapp[ismiss]
  # Step 2(c)
  mss <- mean(((Xna - Xapp)[!ismiss])^2)
  rel_err <- (mssold - mss)/mss0 #relative error
  mssold <- mss
  cat("Iter:", iter, "MSS:", mss,
      "Rel. Err:", rel_err, "\n")
}

#terminates at MSS = 0.391

#compute correlation between 20 imputed values and the actual values
cor(Xapp[ismiss], X[ismiss]) #=0.6535

#this lab implemented an algorithm which is available for use as softImpute package
#on CRAN

##12.5.3 Clustering

#k-means clustering
#simulated data with two true clusters
set.seed(2)
x <- matrix(rnorm(50*2), ncol = 2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4
par(mfrow=c(1,1))
plot(x)

#apply kmeans
km.out <- kmeans(x,2,nstart = 20) #K = 2 (2 clusters)
#assignments to the two clusters
km.out$cluster

#plot results
par(mfrow = c(1,2))
plot(x,col = (km.out$cluster + 1),
     main = "K-Means Clustering Results with K = 2",
     xlab = "", ylab= "", pch = 20, cex = 2)
#if more than two predictors than could have used PCA to reduce down to two dimensions
#(2 PCs)

#in real data, ideal number of clusters is unknown
#try K = 3
set.seed(4)
km.out <- kmeans(x,3,nstart = 20)
km.out

#plot results
plot(x, col = (km.out$cluster + 1),
     main = "K-Means Clustering Results with K = 3",
     xlab = "", ylab = "", pch = 20, cex = 2)

#nstart > 1 then K-means clustering will be performed using multiple random assignments
#comparison of different nstart values
set.seed(4) #important because initial cluster assignments are random
km.out <- kmeans(x, 3, nstart = 1)
km.out$tot.withinss #=104.3319 (total within-cluster sum of squares where the goal is to minimize)
km.out <- kmeans(x,3,nstart = 20)
km.out$tot.withinss #97.9793
#individual within-cluster sum-of-squares are contained in the vector km.out$withinss

#recommendation: run kmeans with nstart of 20 or 50 to avoid undesirable local optimum

#use hierarchical clustering with complete, single, and average linkage clustering with Euclidean distance
#as the dissimilarity measure
#use same simulated data
hc.complete <- hclust(dist(x), method = "complete")
hc.average <- hclust(dist(x), method = "average")
hc.single <- hclust(dist(x), method = "single")

#plot results
par(mfrow = c(1,3))
plot(hc.complete, main = "Complete Linkage",
     xlab = "", sub= "", cex = .9)
plot(hc.average, main = "Average Linkage",
     xlab = "", sub = "", cex = .9)
plot(hc.single, main = "Single Linkage",
     xlab="", sub="", cex = .9)

#cluster assignments for 2 clusters
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2) #assigns one point to own cluster (bad clustering)
#better with four clusters
cutree(hc.single, 4)

#to scale before hierarchical clustering
xsc <- scale(x)
par(mfrow = c(1,1))
plot(hclust(dist(xsc), method = "complete"),
     main = "Hierarchical Clustering with Scaled Features")

#can also use correlation-based distance (which only makes sense with p > 2,
#since the absolute correlation between any two observations with measurements
#on two features is always 1)
x <- matrix(rnorm(30*3), ncol = 3)
dd <- as.dist(1 - cor(t(x)))
plot(hclust(dd, method = "complete"),
     main = "Complete Linkage with Correlation-Based Distance",
     xlab = "", sub = "")

##12.5.4 NCI60 Data Example

library(ISLR2)
nci.labs <- NCI60$labs #cancer type, not wanted for unsupervised learning
nci.data <- NCI60$data 
dim(nci.data) #64 6830

#cancer types
nci.labs[1:4]
table(nci.labs)

#PCA with scaling
pr.out <- prcomp(nci.data, scale = TRUE)

#function to assign color to each of the 64 cell lines
Cols <- function(vec){
  cols <- rainbow(length(unique(vec))) #rainbow takes a positive integer and returns a vector containing number of distinct colors
  return(cols[as.numeric(as.factor(vec))])
  }

#plot principal component score vectors
par(mfrow = c(1,2))
plot(pr.out$x[, 1:2], col = Cols(nci.labs), pch = 19,
     xlab = "Z1", ylab = "Z2")
plot(pr.out$x[,c(1,3)], col = Cols(nci.labs), pch = 19,
     xlab = "Z1", ylab = "Z3")

#std. deviation, proportion of variance, and cumulative proportion for each PC
summary(pr.out)
#plot variance (height of each bar is given by squaring pr.out$sdev)
par(mfrow = c(1,1))
plot(pr.out)

#plot scree plot and cumulative PVE
pve <- 100*pr.out$sdev^2 /sum(pr.out$sdev^2)
par(mfrow = c(1,2))
plot(pve, type = "o", ylab = "PVE", xlab = "Principal COmponent", col = "blue")
plot(cumsum(pve), type = "o", ylab = "Cumulative PVE", xlab = "Principal Component",
     col = "brown3")

#alternatively, PVE can be computed
summary(pr.out)$importance[1,]
#alternatively, cumsum(PVE) can be computer
summary(pr.out)$importance[3,]

#first seven PC explain around 40% of the variance in the data and there is
#an elbow in the scree plot after approximately seventh principal component
#indicates little benefit from examining more than 7 PCs

#use hierarchical clustering to find out whether or not the observations cluster
#into distinct cancer types

#standardize variables (mean = 0, std = 1)
sd.data <- scale(nci.data)

#hierarchical clustering using complete, single, average linkage
#Euclidean distance used for dissimilarity measure
par(mfrow = c(1,3))
data.dist <- dist(sd.data)
plot(hclust(data.dist), xlab = "", sub = "", ylab = "", labels = nci.labs,
     main = "Complete Linkage")
plot(hclust(data.dist, method = "average"), labels = nci.labs, main = "Average Linkage",
     xlab = "", sub = "", ylab = "")
plot(hclust(data.dist, method = "single"),
     labels = nci.labs, main = "Single Linkage",
     xlab = "", sub = "", ylab = "")

#very different clustering
#single typically creates "trailing clusters" where large clusters are attached
#onto individual observations one-by-one
#complete and average yield better balanced clustering and, for this reason,
#are often preferred
#use complete linkage for following analaysis

#cut dendogram to yield particular number of clusters
hc.out <- hclust(dist(sd.data))
hc.clusters <- cutree(hc.out, 4) #4 clusters
table(hc.clusters, nci.labs) #all leukemia cell  lines fall into cluster 3,
#but breast cancer cell lines are spread out

#plot the cut on the dendogram
par(mfrow = c(1,1))
plot(hc.out, labels = nci.labs)
abline(h = 139, col = "red")

#summary of clustering object
hc.out

#try k-means clustering with K=4 to compare
set.seed(2)
km.out <- kmeans(sd.data, 4, nstart = 20)
km.clusters <- km.out$cluster
table(km.clusters, hc.clusters) #different clusters except cluster 3 and 4

#perform hierarchical clustering on first few principal components score vectors
hc.out <-hclust(dist(pr.out$x[,1:5]))
plot(hc.out, labels = nci.labs,
     main = "Hier. Clust. on First Five Score Vectors")
table(cutree(hc.out,4), nci.labs) #different results

#sometimes performing clustering (either k-means or hierarchical) on principal
#components can give better results (it can denoise)
