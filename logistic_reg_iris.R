# https://michael.hahsler.net/SMU/EMIS7331/R/logistic_regression.html
data(iris)
x <- iris[sample(1:nrow(iris)),]
x <- cbind(x, useless = rnorm(nrow(x)))

x$virginica <- x$Species == "virginica"
x$Species <- NULL
plot(x, col=x$virginica+1)
#
model <- glm(virginica ~ ., family = binomial(link='logit'), data=x)
summary(model)
# Do Stepwise Variable Selection
model2 <- step(model, data = x)
summary(model2)

pr <- predict(model2, x, type="response")
round(pr, 2)

hist(pr, breaks=20)
hist(pr[x$virginica==TRUE], col="red", breaks=20, add=TRUE)
#
table(actual=x$virginica, predicted=pr>.5)

# Another version----
# http://rstudio-pubs-static.s3.amazonaws.com/230133_fb9cb3c35ca345d0b9e5726ed3f45a21.html
ir_data<- iris
head(ir_data)
str(ir_data)
levels(ir_data$Species)
ir_data<-ir_data[1:100,]
set.seed(100)
samp<-sample(1:100,80)
ir_train<-ir_data[samp,]
ir_test<-ir_data[-samp,]

library(ggplot2) 
library(GGally)

ggpairs(ir_train)
y<-ir_train$Species
x<-ir_train$Sepal.Length
glfit<-glm(y~x, family = 'binomial')

summary(glfit)

newdata<- data.frame(x=ir_test$Sepal.Length)
predicted_val<-predict(glfit, newdata, type="response")
prediction<-data.frame(ir_test$Sepal.Length, ir_test$Species, predicted_val)
prediction

qplot(prediction[,1], round(prediction[,3]), col=prediction[,2], 
      xlab = 'Sepal Length', ylab = 'Prediction using Logistic Reg.')

table(actual=ir_test$Species, predicted=predicted_val>.5)
