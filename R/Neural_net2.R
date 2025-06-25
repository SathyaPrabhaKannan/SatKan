###############Neural Net#################
#Get data

df <- read.csv('bank_note_data.csv')
head(df)
str(df)

###########################################################3
#Train test split
library(caTools)
set.seed(101)
split = sample.split(df$Class, SplitRatio = 0.70)

train = subset(df, split == TRUE)
test = subset(df, split == FALSE)

str(train)

######################################################
#Neural net
library(neuralnet)
nn <- neuralnet(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy,data=train,hidden=10,linear.output=FALSE)

#Prediction
pred.nn.values <- compute(nn,test[,1:4])
head(pred.nn.values$net.result)
###ROUND###
predictions <- sapply(pred.nn.values$net.result,round)
head(predictions)

table(predictions,test$Class)

#############################################################
#compare with random forest
df$Class <- factor(df$Class)
library(caTools)
set.seed(101)
split = sample.split(df$Class, SplitRatio = 0.70)

train = subset(df, split == TRUE)
test = subset(df, split == FALSE)

#randomforest
rf.model <- randomForest(Class ~.,data = train)

#Prediction
rf.pred <-predict(rf.model,test)
table(rf.pred,test$Class)