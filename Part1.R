library(fpc)

library(cluster)

library(rgl)


dataset1=read.csv("dataset1.csv")
dataset1$cluster=NULL

#distance based
km <-kmeans(dataset1,8)
plot3d(dataset1, col=km$cluster)

#graphbased
set.seed(1134)
library(optpart)
ah <-agnes(dataset1)
ahc <- cutree(ah, 8)
plot3d(dataset1, col=ahc)

#density based
ds <- dbscan(dataset1,.1, MinPts=1)
plot3d(dataset1, col=ds$cluster)
