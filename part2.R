dataset1=read.csv("dataset1.csv")
dataset1$cluster=NULL


library(fpc)


euclid <- function(points1, points2) {
  distanceMatrix <- matrix(NA, nrow=dim(points1)[1], ncol=dim(points2)[1])
  weights <- c(4,2,1)
  for(i in 1:nrow(points2)) {
    distanceMatrix[,i] <- sqrt(rowSums(t(t(points1)-points2[i,])^2)*weights)
  }
  distanceMatrix
}


K_means <- function(x, centers, distFun, nItter) {
  clusterHistory <- vector(nItter, mode="list")
  centerHistory <- vector(nItter, mode="list")
  
  for(i in 1:nItter) {
    distsToCenters <- distFun(x, centers)
    clusters <- apply(distsToCenters, 1, which.min)
    centers <- apply(x, 2, tapply, clusters, mean)
    # Saving history
    clusterHistory[[i]] <- clusters
    centerHistory[[i]] <- centers
  }
  
  list(clusters=clusterHistory, centers=centerHistory)
}

ktest=as.matrix(dataset1) 
centers <- ktest[sample(nrow(ktest), 8),]

res <- K_means(ktest, centers, euclid, 10)
plot3d(dataset1, col=res$clusters[[10]])
