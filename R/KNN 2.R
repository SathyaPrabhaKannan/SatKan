##############################KNN######################################
#Get data

library(ISLR)
str(iris)
summary(iris)

#####################################################
#Standardize /scale the data
stand.features <- scale(iris[1:4])
var(stand.features[,1])
final.data <- cbind(stand.features,iris[5])
print(head(final.data))

######################################################
#Train test split
set.seed(101)

library(caTools)

sample <- sample.split(final.data$Species, SplitRatio = .70)
train <- subset(final.data, sample == TRUE)
test <- subset(final.data, sample == FALSE)

#####################################################################
#KNN model
library(class)
predicted.species <- knn(train[1:4],test[1:4],train$Species,k=1)
print(predicted.species)

miserror.error <- mean(test$Species != predicted.species)

#######################################################################
#Choose K value
predicted.species <- NULL
error.rate <- NULL

for(i in 1:10){
  set.seed(101)
  predicted.species <- knn(train[1:4],test[1:4],train$Species,k=i)
  error.rate[i] <- mean(test$Species != predicted.species)
}

print(error.rate)

#############################################################################
#data visualization
library(ggplot2)
k.values <- 1:10
error.df <- data.frame(error.rate,k.values)
pl <- ggplot(error.df,aes(x=k.values,y=error.rate)) + geom_point() + geom_line(lty="dotted",color='red')
pl