
# Linear discriminant analysis on iris data set ----
# https://rpubs.com/pranaugi011089/98288
library(MASS) #Load package 'MASS' to perform LDA
library(dplyr)
library(tidyverse)
library(ggplot2)
fit.LDA = lda( Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, iris)
fit.LDA

#Perform classification
fit.LDA.C = predict(fit.LDA, newdata=iris[,c(1,2,3,4)])$class
fit.LDA.C

table(iris[,5],fit.LDA.C)

# we’ll use just two species of irises, Iris Setosa and Iris Versicolor.
#iris <- as_tibble(iris)
data(iris)
iris.wrangled <- iris %>% filter(Species == "setosa"| Species == "versicolor") 

iris.wrangled <- iris.wrangled[,c(2,4,5)]

iris.wrangled %>% ggplot(aes(Sepal.Width, Petal.Width, shape = Species)) +
                  geom_point(aes(color = Species), size = 2) +
                  xlim(0,5)+
                  ylim(0,2)

fit2.LDA = lda(Species ~., data = iris.wrangled)
fit2.LDA

fit2.LDA.C = predict(fit2.LDA, newdata=iris.wrangled)$class
fit2.LDA.C

table(iris.wrangled[,3], fit2.LDA.C)
