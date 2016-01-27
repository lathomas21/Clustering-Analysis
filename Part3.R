dataset2=read.csv("dataset2.csv") [-1]


library(caret)
set.seed(1134)
test <- createDataPartition(dataset2$class, p = .001,
                                  list = FALSE,
                                  times = 1)
datatest <- dataset2[test,]
datatest$class=NULL

library(cluster)

library(GMD)
dist.obj <- dist(datatest)
hclust.obj <- hclust(dist.obj)
css.obj <- css.hclust(dist.obj, hclust.obj)
elbow.batch(css.obj)

l <- fanny(datatest, 9)
plot(datatest, col= l$cluster)

hist(l$clustering)


