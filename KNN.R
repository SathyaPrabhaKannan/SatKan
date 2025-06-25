#####################KNN###########################
#Get data
library(ISLR)
str(Caravan)
summary(Caravan$Purchase)

############################
#Data cleaning
#Check the variance
var(Caravan[,1])
var(Caravan[,2])

#Normalize/scale the variables
purchase <- Caravan[,86]
standardized.Caravan <- scale(Caravan[,-86])

var(standardized.Caravan[,1])
var(standardized.Caravan[,2])

###################################################
#Train test 
test.index <- 1:1000
test.data <- standardized.Caravan[test.index,]
test.purchase <- purchase[test.index]

train.data <- standardized.Caravan[-test.index,]
train.purchase <- purchase[-test.index]

####################################################
#KNN
library(class)
set.seed(101)
predicted.purchase <- knn(train.data,test.data,train.purchase,k=1)
head(predicted.purchase)

misclasserror <- mean(test.purchase != predicted.purchase)
misclasserror

#Choose K value
predicted.purchase = NULL
error.rate = NULL

for(i in 1:20){
  set.seed(101)
  predicted.purchase <- knn(train.data,test.data,train.purchase,k=i)
  error.rate[i] <- mean(test.purchase != predicted.purchase)
}

print(error.rate)

#####################################################
#Visualize K elbow method
library(ggplot2)
k.values <- 1:20
error.df <- data.frame(error.rate,k.values)
error.df

ggplot(error.df,aes(x=k.values,y=error.rate)) + geom_point()+ geom_line(lty="dotted",color='red')
