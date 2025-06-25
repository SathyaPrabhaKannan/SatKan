############################Logistic regression###########################
#Get data

adult <- read.csv('adult_sal.csv')
head(adult_sal)

########################################################
#drop 1 index column
library(dplyr)
adult <- select(adult, -X)

print(str(adult))
print(summary(adult))
print(head(adult))

#################################################################
#Data cleaning
print(table(adult$type_employer))

#combine employer type function
unemp <- function(job){
  job <- as.character(job)
  if (job=='Never-worked' | job=='Without-pay'){
    return('Unemployed')
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer,unemp)
print(table(adult$type_employer))

################################################################
#combine self-emp group function
group_emp <- function(job){
  if (job=='Local-gov' | job=='State-gov'){
    return('SL-gov')
  }else if (job=='Self-emp-inc' | job=='Self-emp-not-inc'){
    return('self-emp')
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer,group_emp)
print(table(adult$type_employer))

print(table(adult$marital))

#group marital function
group_marital <- function(mar){
  mar <- as.character(mar)
  
  # Not-Married
  if (mar=='Separated' | mar=='Divorced' | mar=='Widowed'){
    return('Not-Married')
    
    # Never-Married   
  }else if(mar=='Never-married'){
    return(mar)
    
    #Married
  }else{
    return('Married')
  }
}

adult$marital <- sapply(adult$marital,group_marital)
print(table(adult$marital))


#country
print(table(adult$country))

Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

#Group_country function
group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}

adult$country <- sapply(adult$country,group_country)
print(table(adult$country))

###########################################################################
#Missing values
library(Amelia)
adult[adult=='?'] <- NA

########################################################################
#Factor
adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)
adult$education <- sapply(adult$education,factor)
adult$occupation <- sapply(adult$occupation,factor)
adult$sex <- sapply(adult$sex,factor)
adult$race <- sapply(adult$race,factor)


print(table(adult$type_employer))

####################################################################
#drop missing value
adult <- na.omit(adult)

missmap(adult)
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

########################################################################
#EDA
library(ggplot2)
library(dplyr)

ggplot(adult,aes(age)) + geom_histogram(aes(fill=income),color = 'black',binwidth = 1) +theme_dark()
ggplot(adult,aes(hr_per_week)) + geom_histogram() + theme_bw()


#rename col name country to region
adult <- rename(adult,region = country)
print(str(adult))

pl <- ggplot(adult,aes(region)) + geom_bar(aes(fill = income),color = 'black') + theme_bw()
print(pl)

########################################################################
#Train test split
library(caTools)
set.seed(101)
sample <- sample.split(adult$income,SplitRatio = 0.7)
train <- subset(adult,sample ==T)
test <- subset(adult,sample == F)
