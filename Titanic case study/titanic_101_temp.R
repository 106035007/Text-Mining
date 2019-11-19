# remove variables from memory
rm(list=ls())
# import data
library(tidyverse)
setwd('../Titanic case study/')
#train <- read_csv('train.csv')
train <- read_csv('../Titanic case study/train.csv')

str(train)

table(train$Survived)

table(train$Survived, train$Sex)
prop.table(table(train$Sex, train$Survived), margin=1)

# Check the data if there is any NA value
colSums(is.na(train))
table(train$Embarked)

#train$Embarked[train$Embarked == NA]
sum(is.na(train$Embarked))
train[is.na(train$Embarked), "Embarked"] <- 'S'
train1 <- train
colSums(is.na(train1))

# 
library(rpart)
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked, 
                       data = train[!is.na(train$Age),], method = 'anova')

train$Age[is.na(train$Age)] <- predict(predicted_age, 
                                      train[is.na(train$Age),])

# import test data
test <- read_csv('test.csv') 
str(test)
colSums(is.na(test))
# Combine train and test dataset
test$Survived <- NA
str(test)
test1 <- test

full <- rbind(train1, test1)
str(full)

colSums(is.na(full))
#
full[!complete.cases(full$Age), ]

# Clean 1 missing in Fare
full[!complete.cases(full$Fare),]
full$Fare[1044] <- median(full$Fare, na.rm = TRUE)
