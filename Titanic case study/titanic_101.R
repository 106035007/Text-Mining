# http://www.networkx.nl/programming/titanic-machine-learning-from-disaster-part-1/
# https://www.kaggle.com/c/titanic
# https://trevorstephens.com/kaggle-titanic-tutorial/r-part-1-booting-up/
# https://trevorstephens.com/kaggle-titanic-tutorial/r-part-3-decision-trees/
# How to do the Titanic Kaggle competition in R - Part 1
# https://www.youtube.com/watch?v=Zx2TguRHrJE
# Part 1:
# Data Exploration and basic Model Building

rm(list=ls())

#setwd('../DataScience/Titanic case study/')
train <- read.csv("train.csv")
test <- read.csv("test.csv")

str(train)
str(test)
head(train,2)
head(test,2)

# The training set has 891 observations and 12 variables and the testing set has 418 observations and 11 variables. 
# The traning set has 1 extra varible. Check which which one we are missing. I know we could see that in 
#a very small dataset like this, but if its larger we want two compare them.

colnames_check <- colnames(train) %in% colnames(test)
colnames(train[colnames_check==FALSE])

table(train$Survived) 
prop.table(table(train$Survived))

table(train$Sex, train$Survived)
prop.table(table(train$Sex, train$Survived),margin = 1)

#train$Child <- 0
#train$Child[train$Age < 18] <- 1

#aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
#aggregate(Survived ~ Child + Sex, data=train, FUN=length)
#aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# Model Building
# First prediction – All Female Survived
test_female <- test
test_female$Survived <- 0
test_female$Survived[test_female$Sex == "female"] <- 1

# Create a data frame with two columns: PassengerId & Survived and write the solution away to a csv file.
my_solution <- data.frame(PassengerId = test_female$PassengerId, Survived = test_female$Survived)
# write.csv(my_solution, file =  "all_female_survive.csv", row.names = FALSE)

# Clean up the dataset
colSums(is.na(train))
colSums(is.na(test))

# To tackle the missing values I’m going to predict the missing values with the full data set. 
# First we need to combine the test and training set together.
train2 <- train
test2 <- test
test2$Survived <- NA
full <- rbind(train2, test2)

#
# First we tackle the missing Fare, because this is only one value. Let see in wich row it’s missing.
full[!complete.cases(full$Fare),]

# As we can see the passenger on row 1044 has an NA Fare value. Let’s replace it with the median fare value.
full$Fare[1044] <- median(full$Fare, na.rm = TRUE)

# We make a prediction of a passengers Age using the other variables and a decision tree model.
# This time we give method = “anova” since you are predicting a continuous variable.

library(rpart)
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                       data = full[!is.na(full$Age),], method = "anova")
full$Age[is.na(full$Age)] <- predict(predicted_age, full[is.na(full$Age),])

# split back to original data set
train2 <- full[1:891,]
test2 <- full[892:1309,]

# Build a Decision Tree with rpart
my_dt1 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
                data = train2, 
                method = "class")
plot(my_dt1)
text(my_dt1)
# Load in the packages to create a fancified visualized version of your tree.
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(my_dt1)
# The root node, at the top, shows 62% of passengers die, while 38% survive. 
# The number above these proportions indicates the way that the node is voting 
# (recall we decided at this top level that everyone would die, or be coded as zero) and 
# the number below indicates the proportion of the population that resides in this node, or bucket 
# (here at the top level it is everyone, 100%).

# If the passenger was male, only 19% survive, so the bucket votes that everyone here 
# (65% of passengers) perish, while the female bucket votes in the opposite manner, most of them survive 
# as we saw before.
# Move to the right side: Given sex = female, then if Pclass >= 2.5, then death ratio is 50%.
# If Pclass<2.5, then survival ratio is 95%.

Prediction <- predict(my_dt1, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
# write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

# Categorical casting
str(full)
full$Pclass <- as.factor(full$Pclass)

table(full$Embarked)
full[full$Embarked == '', ]
full[full$Embarked == '', 'Embarked'] <- 'S'
table(full$Embarked)

# split back to original data set
train2 <- full[1:891,]
test2 <- full[892:1309,]
train2$Survived <- as.factor(train2$Survived)

test2$Survived
test2$Survived[is.na(test2$Survived)] <- 0

# randomForest
library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(ranger)       # a faster implementation of randomForest
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)          # an extremely fast java-based platform

survival.equation <- 'Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked'
survival.formula <- as.formula(survival.equation)
model1_rf <- randomForest(survival.formula, data = train2, mtree = 500, mtry = 3, nodesize = 0.01*nrow(train2))
#
feature.equation <- 'Pclass + Sex + Age + SibSp + Parch + Fare + Embarked'
#
Survived <- predict(model1_rf, newdata = test2)
PassengerID <- test2$PassengerId
output <- as.data.frame(PassengerID)
output$Survived <- Survived
