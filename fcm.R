require(ppclust)
require(factoextra)
require(dplyr)
require(cluster)
require(fclust)
data("iris3")
x=iris[,-5]

pairs(x, col=iris[,5])
cor(iris[,1:4])
#require(psych)
#pairs.panels(iris[,-5], method = "pearson")
res.fcm <- fcm(x, centers=3)
summary(res.fcm)