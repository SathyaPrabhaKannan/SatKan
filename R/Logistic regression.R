#################Logistic Regression#######################
#Get data
df.train <- read.csv('titanic_train.csv')
print(head(df.train))
print(str(df.train))
library(Amelia)

##############################################
#Impute of age based on class
impute_age <- function(age,class){
  out <- age
  for (i in 1:length(age)){
    
    if (is.na(age[i])){
      
      if (class[i] == 1){
        out[i] <- 37
        
      }else if (class[i] == 2){
        out[i] <- 29
        
      }else{
        out[i] <- 24
      }
    }else{
      out[i]<-age[i]
    }
  }
  return(out)
}

fixed.ages <- impute_age(df.train$Age,df.train$Pclass)
df.train$Age <- fixed.ages

missmap(df.train,main = 'mputation check',col = c('yellow','black'),legend=FALSE)

#######################################################
#Build the model
str(df.train)
print(head(df.train,3))

library(dplyr)
df.train <- select(df.train,-PassengerId,-Name,-Ticket,-Cabin)
print(head(df.train,3))

str(df.train)



df.train$Survived <- factor(df.train$Survived)
df.train$Pclass <- factor(df.train$Pclass)
df.train$Parch <- factor(df.train$Parch)
df.train$SibSp <- factor(df.train$SibSp)

str(df.train)

#####################################################
#Train test split
log.model <- glm(formula=Survived ~ . , family = binomial(link='logit'),data = df.train)
print(summary(log.model))


library(caTools)
set.seed(101)

split = sample.split(df.train$Survived, SplitRatio = 0.70)

final.train = subset(df.train, split == TRUE)
final.test = subset(df.train, split == FALSE)

final.log.model <- glm(formula=Survived ~ . , family = binomial(link='logit'),data = final.train)
print(summary(final.log.model))

####################################################
#Predicting test cases

fitted.probabilities <- predict(final.log.model,final.test,type='response')
fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)

#Checking accuracy
misClasificError <- mean(fitted.results != final.test$Survived)
print(1-misClasificError)

table(final.test$Survived, fitted.probabilities > 0.5)
