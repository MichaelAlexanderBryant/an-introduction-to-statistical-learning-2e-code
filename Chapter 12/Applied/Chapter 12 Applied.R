#show how correlation-based distance and Euclidean distance are almost
#equivalent using USArrests data set (1-r_i,j) is proportional to Euclidean
#distance where r_i,j is the correlation between observations i and j

library(ISLR2)
View(USArrests)

set.seed(0)

# Scale each observation (not the features):
USA_scaled <- t(scale(t(USArrests)))

# The correlation of each sample with the other samples:
Rij <- cor(t(USA_scaled))  # -1 <= Rij <= +1 
OneMinusRij <- 1 - Rij  # 0 <= 1-Rij <= +2 
X <- OneMinusRij[lower.tri(OneMinusRij)]

D <- as.matrix(dist(USA_scaled)^2)
Y <- D[lower.tri(D)]

plot(X, Y)

#calculate PVE using built-in R functions and manually
set.seed(1)
#built-in functions
pr.out = prcomp(USArrests, center=T, scale=T)
pr.var = pr.out$sdev^2
pve = pr.var / sum(pr.var)
pve

#manually
#loadings
loadings = pr.out$rotation

#initialize storage array
pve2 = rep(NA, 4)

#mean of each column
dmean = apply(USArrests, 2, mean)

#standard deviation of columns
dsdev = sqrt(apply(USArrests, 2, var))

#subtract column mean from each entry by column
dsc = sweep(USArrests, MARGIN=2, dmean, "-")

#divide difference by standard deviation (standardized)
dsc = sweep(dsc, MARGIN=2, dsdev, "/")

#multiply standardized values by loading
#sum row-wise and square
#store in pve2 and repeat for each prinicpal component
for (i in 1:4) {
  proto_x = sweep(dsc, MARGIN=2, loadings[,i], "*")
  pc_x = apply(proto_x, 1, sum)
  pve2[i] = sum(pc_x^2)
}
#divide the explained variance by the total variance
pve2 = pve2/sum(dsc^2)
pve2

#hierarchical clustering of states in USArrests
#complete linkage and Euclidean distance
plot(hclust(dist(USArrests), method="complete"))
#cut dendogram at 3 distinct clusters
abline(h = 125, col = "red")
#Cluster 1: e.g., florida, california
#Cluster 2: e.g., colorado, oklahoma
#Cluster 3: e.g., vermont, north dakota

#repeat, but scale first
plot(hclust(dist(scale(USArrests)), method="complete"))

#given the difference in scale of the variables, they should be standardized first
#dendograms are slightly different, it looks like creating two clusters would seperate
#data into high and low crime states, and creating 4 clusters would further divide
#this into high crime/high population, high crime/low population, low crime/high populatino,
#and low crime/low population

#simulate data and perform PCA and K-means clustering
sim_data <- rbind(data.frame(matrix( rnorm(20*50,mean=0,sd=1), 20, 50)),
                  data.frame(matrix( rnorm(20*50,mean=2,sd=1), 20, 50)),
                  data.frame(matrix( rnorm(20*50,mean=6,sd=1), 20, 50)))

#perform PCA and plot first two components
pr.out = prcomp(sim_data, center=T, scale=T)
plot(pr.out$x[1:20,1:2],
       xlim = c(-10,10),
       ylim = c(-5,5),
       col = 2)
points(pr.out$x[21:40,1:2], col = 3)
points(pr.out$x[41:60,1:2], col = 4)

#k-means with k=3
km.out <- kmeans(sim_data, 3, nstart = 20)
par(mfrow = c(1,2))
plot(pr.out$x[1:20,1:2],
     xlim = c(-10,10),
     ylim = c(-5,5),
     col = 2)
points(pr.out$x[21:40,1:2], col = 3)
points(pr.out$x[41:60,1:2], col = 4)
plot(pr.out$x[km.out$cluster==1,1:2],
     xlim = c(-10,10),
     ylim = c(-5,5),
     col = 2)
points(pr.out$x[km.out$cluster==2,1:2], col = 3)
points(pr.out$x[km.out$cluster==3,1:2], col = 4)
#PCA visually seperated the different groups along the first prinicpal component
#K-means was able to identify the different groups correctly

#k-means with k =2
km.out <- kmeans(sim_data, 2, nstart = 20)
par(mfrow = c(1,2))
plot(pr.out$x[1:20,1:2],
     xlim = c(-10,10),
     ylim = c(-5,5),
     col = 2)
points(pr.out$x[21:40,1:2], col = 3)
points(pr.out$x[41:60,1:2], col = 4)
plot(pr.out$x[km.out$cluster==1,1:2],
     xlim = c(-10,10),
     ylim = c(-5,5),
     col = 2)
points(pr.out$x[km.out$cluster==2,1:2], col = 3)
#k-means grouped the two "different groups" which were most similar together

#k-means with k = 4
km.out <- kmeans(sim_data, 4, nstart = 20)
par(mfrow = c(1,2))
plot(pr.out$x[1:20,1:2],
     xlim = c(-10,10),
     ylim = c(-5,5),
     col = 2)
points(pr.out$x[21:40,1:2], col = 3)
points(pr.out$x[41:60,1:2], col = 4)
plot(pr.out$x[km.out$cluster==1,1:2],
     xlim = c(-10,10),
     ylim = c(-5,5),
     col = 2)
points(pr.out$x[km.out$cluster==2,1:2], col = 3)
points(pr.out$x[km.out$cluster==3,1:2], col = 4)
points(pr.out$x[km.out$cluster==4,1:2], col = 5)
#k-means selected half of one group to split in two
#this would change depending on the initial assignments

#k-means with k=3 on first two principal components
km.out <- kmeans(pr.out$x[,1:2], 3, nstart = 20)
par(mfrow = c(1,2))
plot(pr.out$x[1:20,1:2],
     xlim = c(-10,10),
     ylim = c(-5,5),
     col = 2)
points(pr.out$x[21:40,1:2], col = 3)
points(pr.out$x[41:60,1:2], col = 4)
plot(pr.out$x[km.out$cluster==1,1:2],
     xlim = c(-10,10),
     ylim = c(-5,5),
     col = 2)
points(pr.out$x[km.out$cluster==2,1:2], col = 3)
points(pr.out$x[km.out$cluster==3,1:2], col = 4)
#k-means correctly identified the three groups using the first two PCs

#scale then k-means with k=3
km.out <- kmeans(scale(sim_data), 3, nstart = 20)
par(mfrow = c(1,2))
plot(pr.out$x[1:20,1:2],
     xlim = c(-10,10),
     ylim = c(-5,5),
     col = 2)
points(pr.out$x[21:40,1:2], col = 3)
points(pr.out$x[41:60,1:2], col = 4)
plot(pr.out$x[km.out$cluster==1,1:2],
     xlim = c(-10,10),
     ylim = c(-5,5),
     col = 2)
points(pr.out$x[km.out$cluster==2,1:2], col = 3)
points(pr.out$x[km.out$cluster==3,1:2], col = 4)
#results of the scaled data are the same as unscaled data

#create a function for matrix completion using Algorithm 12.1 (skipping problems
#11 and 12)

NA.rows <- sample(1:nrow(Boston), .1*nrow(Boston))
NA.columns <- round(runif(length(NA.rows), min=1, max=4),0)
df_copy <- Boston

for (i in 1:length(NA.rows)){
  df_copy[NA.rows[i], NA.columns[i]] <- NA
}


sum(is.na(df_copy))


matrix_comp <- function(df){
  
  impute.avg.X <- df
  
  NA.indices <- which(is.na(df), arr.ind=TRUE)
  NA.mask <- is.na(df)
  
  #step 1
  #matrix with NA values imputed with average of column
  for (i in 1:dim(df)[2])
    impute.avg.X[is.na(df[,i]),i] <- mean(df[,i],na.rm=TRUE)
  
  while (least.squares1 != least.squares2) {
    
    #step 2(a)
    #PCA
    pr.out <- prcomp(df)
    X <- pr.out$x %*% t(pr.out$rotation)
    
    for (i in 1:length(NA.indices[,1])){
      
      row <- NA.indices[i,1]
      col <- NA.indices[i,2]
      
      impute.avg.X[row,col] <- X[row,col]
      
    }
    
    
    
    
    
  }

}


matrix_comp(df_copy)

#use gene expression data set

getwd()
setwd("C:/Users/malex/Documents/Introduction to Statistical Learning/Chapter 12/Applied")

data <- read.csv('Ch12Ex13.csv', head=F)
View(data) #rows are genes (n = 1000) and columns are tissue samples (first 20 healthy,
#second 20 diseased)

#hierarchical clustering with correlation-based distance
par(mfrow=c(1,1))
dd <- as.dist(1 - cor(t(data)))
plot(hclust(dd, method="complete"))
plot(hclust(dd, method="single"))
plot(hclust(dd, method="average"))
plot(hclust(dd, method="centroid"))
#dendograms are different depending on linkage

#to find which genes vary the most across healthy/diseased samples, look at variance
#with PCA loadings
pr.out <- prcomp(t(data), scale = TRUE)


plot(pr.out$x[1:20,1:2],
     xlim = c(-20,20),
     ylim = c(-20,20),
     col = 2)
points(pr.out$x[21:40,1:2], col = 3)

total_load[order(abs(apply(pr.out$rotation, 1, sum)), decreasing = T)[1:10]]
