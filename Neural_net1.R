##################Neural nets#######################
#Get data
library(MASS)
head(Boston)
str(Boston)

data <- Boston

##################################################
#Normalize the data
maxs <- apply(data,2,max)
mins <- apply(data,2,min)

scaled.data <- scale(data, center =mins,scale = maxs-mins)
scaled <- as.data.frame(scaled.data)
head(scaled)

####################################################
#Train test split
library(caTools)
split <- sample.split(scaled$medv,SplitRatio = 0.7)
train <- subset(scaled == T)
test <- subset(scaled == F)


#############################################################
#Neuralnet
library(neuralnet)
#names of columns
n <- names(train)
n
#paste as formula
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
f
#Neural net
nn <- neuralnet(f,data=train,hidden=c(5,3),linear.output=TRUE)

#nn visualization
plot(nn)

###########################################################
#Prediction
pred.nn.values <- compute(nn,test[1:13])
str(pred.nn.values)

true.predictions <- pred.nn.values$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
test.r <- (test$medv)*(max(data$medv)-min(data$medv))+min(data$medv)

#MSE
MSE.nn <- sum((test.r - true.predictions)^2)/nrow(test)
MSE.nn


#Error
error.df <- data.frame(test.r,true.predictions)
head(error.df)

#######################################################
#Visualize
library(ggplot2)
ggplot(error.df,aes(x=test.r,y=true.predictions)) + geom_point() + stat_smooth()

