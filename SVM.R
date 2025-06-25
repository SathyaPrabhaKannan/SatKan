##################Support Vector Machine#######################
#Get data

library(ISLR)
head(iris)

###########################################
#Build the model
model <- svm(Species ~ . , data = iris)
summary(model)

###########################################
#Prediction
pred.values <- predict(model,iris[1:4])
table(pred.values,iris[,5])

######################################################
#Advanced tuning
tune.results <- tune(svm,train.x = iris[1:4],train.y = iris[,5],kernel = 'radial',ranges = list(cost=c(0.1,1,10),gamma=c(0.5,1,2)))
summary(tune.results)


tune.svm <- svm(Species ~.,data = iris, kernel = 'radial', cost = 1.5,gamma = 0.1)
tune.svm
