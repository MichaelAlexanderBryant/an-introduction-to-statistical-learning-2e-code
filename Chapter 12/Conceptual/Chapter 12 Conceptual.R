#hierarchical clustering using dissimilarity matrix, based on linkages
d = as.dist(matrix(c(0,0.3,0.4,0.7,
                     0.3, 0, 0.5, 0.8,
                     0.4, 0.5, 0, 0.45,
                     0.7, 0.8, 0.45, 0), nrow = 4))
plot(hclust(d, method="complete"))
plot(hclust(d, method="single"))

#cut complete linkage dendogram such that there are two clusters
plot(hclust(d, method="complete"))
abline(h = 0.7, col = "red") #cluster 1 contains 1 and 2, cluster 2 contains 3 and 4

#cut single linkage dendogram such that there are two clusters
plot(hclust(d, method="single"))
abline(h = 0.44, col = "red") #cluster 1 contains 4, cluster 2 contains 1,2, and 3


#manual k-means clustering
#data, plot, and random sample to initialize two clusters
data_prob_3 <- data.frame(X1=c(1,1,0,5,6,4), X2=c(4,3,4,1,2,0))
plot(x=data_prob_3[,'X1'], y=data_prob_3[,'X2'])
random_sample <- sample(1:nrow(data_prob_3), 3) #2,6,5

#calculate initial clusters
centroid_1 <- c(sum(data_prob_3[random_sample,1])/3,sum(data_prob_3[random_sample,2])/3)
centroid_2 <- c(sum(data_prob_3[-random_sample,1])/3,sum(data_prob_3[-random_sample,2])/3)

#plot initial clusters
plot(x=data_prob_3[,'X1'], y=data_prob_3[,'X2'])
points(x=centroid_1[1], y=centroid_1[2],col="red")
points(x=centroid_2[1], y=centroid_2[2],col="green")

#calculate euclidean distance to initial clusters, assign data to initial clusters
for (i in 1:6){
  euclidean_dist_1 <- sqrt((data_prob_3[i,1]-centroid_1[1])^2 + (data_prob_3[i,2]-centroid_1[2])^2)
  euclidean_dist_2 <- sqrt((data_prob_3[i,1]-centroid_2[1])^2 + (data_prob_3[i,2]-centroid_2[2])^2)
  
  if (euclidean_dist_1 < euclidean_dist_2) {
    data_prob_3$cluster[i] <- 1
  }
  
  else {
    data_prob_3$cluster[i] <- 2
  }
  
}

#repeat the following until clusters don't move

#calculate new centroids
centroid_1 <- c(sum(data_prob_3[data_prob_3$cluster == 1,1])/3,sum(data_prob_3[data_prob_3$cluster == 1,2])/3)
centroid_2 <- c(sum(data_prob_3[data_prob_3$cluster == 2,1])/3,sum(data_prob_3[data_prob_3$cluster == 2,2])/3)

#plot data and new centroids
plot(x=data_prob_3[,'X1'], y=data_prob_3[,'X2'])
points(x=centroid_1[1], y=centroid_1[2],col="red")
points(x=centroid_2[1], y=centroid_2[2],col="green")

#calculate euclidean distance for new centroids, assign new clusters
for (i in 1:6){
  euclidean_dist_1 <- sqrt((data_prob_3[i,1]-centroid_1[1])^2 + (data_prob_3[i,2]-centroid_1[2])^2)
  euclidean_dist_2 <- sqrt((data_prob_3[i,1]-centroid_2[1])^2 + (data_prob_3[i,2]-centroid_2[2])^2)
  
  if (euclidean_dist_1 < euclidean_dist_2) {
    data_prob_3$cluster[i] <- 1
  }
  
  else {
    data_prob_3$cluster[i] <- 2
  }
  
}


