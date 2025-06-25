######################Linear regression#######################
#Get data
df <- read.csv('student-mat.csv',sep=';')
head(df)
summary(df)

###################################
#Data cleaning
any(is.na(df))
str(df)

###################################
#EDA
#For correlation
num.cols <- sapply(df, is.numeric)

cor.data <- cor(df[,num.cols])

cor.data

library(corrplot)
library(corrgram)

####################################
#Data visualization
corrplot(cor.data,method='color')

corrgram(df,order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)

#To predict G3
ggplot(df,aes(x=G3)) + geom_histogram(bins=20,alpha=0.5,fill='blue') + theme_grey()

########################################################
#train test split
library(caTools)
set.seed(101) 
sample <- sample.split(df$age, SplitRatio = 0.70) 
train = subset(df, sample == TRUE)
test = subset(df, sample == FALSE)

model <- lm(G3 ~ .,train)
summary(model)

#############################################
#Visualize model
res <- residuals(model)
res <- as.data.frame(res)
head(res)

#ggplot
ggplot(res,aes(res)) +  geom_histogram(fill='blue',alpha=0.5)
plot(model)

############################################
#Predictions
G3.predictions <- predict(model,test)
results <- cbind(G3.predictions,test$G3) 
colnames(results) <- c('pred','actual')
results <- as.data.frame(results)

#Function to deal with negative value
to_zero <- function(x){
  if  (x < 0){
    return(0)
  }else{
    return(x)
  }
}

#Apply zero function
results$pred <- sapply(results$pred,to_zero)

###################################################
#mse
mse <- mean((results$real-results$pred)^2)
print(mse)

#Rmse
mse^0.5

SSE = sum((results$pred - results$real)^2)
SST = sum( (mean(df$G3) - results$real)^2)

R2 = 1 - SSE/SST
R2
