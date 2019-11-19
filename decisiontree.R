# Decision Tree with R | Complete Example----
# https://www.youtube.com/watch?v=tU3Adlru1Ng
#Read data file
mydata <- read.csv("../DataScience/Cardiotocographic.csv")
str(mydata)
mydata$NSPF <- as.factor(mydata$NSP)
# partition data into training data and validation data set
set.seed(1234)
pd <- sample(1:2, nrow(mydata), replace=TRUE, prob = c(0.8, 0.2))
train <- mydata[pd==1,]
validate <- mydata[pd==2,]
#Decision tree with party
library(party)
tree <- ctree(NSPF~LB+AC+FM, data = train)
tree
plot(tree)

mytree <- ctree(NSPF~LB+AC+FM, mydata, controls=ctree_control(mincriterion=0.99, minsplit=500))
print(mytree)
plot(mytree,type="simple")

#Misclassification error
tab<-table(predict(mytree), mydata$NSPF)
print(tab)
1-sum(diag(tab))/sum(tab)

# Decision tree example: iris----
# https://www.youtube.com/watch?v=JFJIQ0_2ijg
# https://www.youtube.com/watch?v=6Ht-nIf_NKc
library(rpart)
library(rpart.plot)

iris
str(iris)

dim(iris)
s <- sample(150, 100)
iris_train <- iris[s, ]
iris_test <- iris[-s, ]
dtm <- rpart(Species ~., iris_train, method = "class")

rpart.plot(dtm, type = 4, extra = 101)

p <- predict(dtm, iris_test, type = "class")
table(iris_test[,5], p)

