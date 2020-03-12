require(ppclust)
#require(factoextra)
require(dplyr)
require(cluster)
require(fclust)
library(caret)
data("iris3")
x=iris[,-5]

pairs(x, col=iris[,5])
cor(iris[,1:4])
#require(psych)
#pairs.panels(iris[,-5], method = "pearson")
res.fcm <- fcm(x, centers=3)
as.data.frame(res.fcm$u)[1:6,]
#summary(res.fcm)
iris[,6] = res.fcm$cluster
df = as.data.frame(iris)
df[which(df[6]==1),6] = "versicolor"
df[which(df[6]==2),6] = "setosa"
df[which(df[6]==3),6] = "virginica"
print(precision(table(df[,5],df[,6]),relevant = "setosa"))
confusionMatrix(table(df[,5],df[,6]))
F_meas(table(df[,5],df[,6]),relevant = "virginica")
