##################DT &RF##########################
#Get data
install.packages("rpart")
library(rpart)

str(kyphosis)

###############################################
#tree model
tree <- rpart(Kyphosis ~ .,method = "class",data = kyphosis)

printcp(tree)

#tree visualization
plot(tree,uniform = T, main = 'Kyphosis Tree')

text(tree,use.n = T, all = T)

##########################################
#plot
install.packages("rpart.plot")
library(rpart.plot)
prp(tree)

################################################
#Random forest
install.packages("randomForest")
library(randomForest)


rf.model <- randomforest(Kyphosis ~ .,data = kyphosis)
print(rf.model)


rf.model$call
rf.model$confusion





