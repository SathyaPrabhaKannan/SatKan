###################K-means clustering####################
#Get data

library(ISLR)
print(head(iris))

######################################################
#data visualization
library(ggplot2)
pl <- ggplot(iris,aes(Petal.Length,Petal.Width,color = Species))
print(pl + geom_point(size = 4))

#####################################################################
#K-means cluster
set.seed(101)
iriscluster <- kmeans(iris[,1:4],3,nstart = 20)
print(iriscluster)

table(iriscluster$cluster,iris$Species)

###################################################################
#Clusplot

library(clusplot)
clusplot(iris,iriscluster$cluster,color = T, shade = T,labels =0 ,lines = 0)