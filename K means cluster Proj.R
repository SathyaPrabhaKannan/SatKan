#########################K-means clustering############################
#Get data

df1 <- read.csv('winequality-red.csv',sep = ';')
df2 <- read.csv('winequality-white.csv',sep = ';')

#######################################################################
#Adding label
df1$label <- sapply(df1$pH,function(x){'Red'})
df2$label <- sapply(df2$pH,function(x){'White'})

head(df1)
head(df2)

#####################################################################
#Combine df1 and df2 into single dataframe 'wine'
wine <- rbind(df1,df2)
str(wine)

######################################################################
#EDA
library(ggplot2)
pl <- ggplot(wine,aes(residual.sugar)) + geom_histogram(aes(fill = label),color = 'black',bins =50)
pl + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_dark()

pl <- ggplot(wine,aes(citric.acid)) + geom_histogram(aes(fill = label),color = 'black',bins =50)
pl + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_dark()


pl <- ggplot(wine,aes(alcohol)) + geom_histogram(aes(fill = label),color = 'black',bins =50)
pl + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_dark()

pl <- ggplot(wine,aes(citric.acid,residual.sugar)) + geom_point(aes(color = label),alpha =0.3)
pl + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_dark()

pl <- ggplot(wine,aes(volatile.acidity,residual.sugar)) + geom_point(aes(color = label),alpha =0.3)
pl + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_dark()


#############################################################################
#K-means cluster
clus.data <- wine[1:12]
head(clus.data)
wine.cluster <- kmeans(clus.data,2)
print(wine.cluster$centers)

table(wine$label,wine.cluster$cluster)






