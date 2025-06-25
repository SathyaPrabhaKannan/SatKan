###########################DT & RF##############################
#Get data

library(ISLR)
df <- College

###########################################################################
#Visualize
library(ggplot2)
ggplot(df,aes(Room.Board,Grad.Rate)) + geom_point(aes(color=Private),size = 4,alpha = 0.4)

ggplot(df,aes(F.Undergrad)) + geom_histogram(aes(fill=Private),color='black',bins=50)
ggplot(df,aes(Grad.Rate)) + geom_histogram(aes(fill=Private),color='black',bins=50) + theme_bw()

#Grad>100
subset(df,Grad.Rate > 100)
df['Cazenovia College','Grad.Rate'] <- 100
subset(df,Grad.Rate > 100)

###############################################################
#Train test split
library(caTools)

set.seed(101) 

sample = sample.split(df$Private, SplitRatio = .70)
train = subset(df, sample == TRUE)
test = subset(df, sample == FALSE)

###############################################################
#Decision Tree
library(rpart)
tree <- rpart(Private ~.,method='class',data = train)
summary(tree)

tree.preds <- predict(tree,test)
head(tree.preds)

tree.preds <- as.data.frame(tree.preds)

# Function using if
joiner <- function(x){
  if (x>=0.5){
    return('Yes')
  }else{
    return("No")
  }
}

tree.preds$Private <- sapply(tree.preds$Yes,joiner)

head(tree.preds)

#########################################################################
#confusion matrix
table(tree.preds$Private,test$Private)

##########################################################################
#plot tree mdel
library(rpart.plot)
prp(tree)

##########################################################################
#random forest
library(randomForest)
rf.model<- randomForest(Private ~.,data = train, importance = TRUE )
rf.model$confusion
rf.model$importance

############################################################################
#Prediction

rf.preds <- predict(rf.model,test)
table(rf.preds,test$Private)
