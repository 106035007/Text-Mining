# Reference: https://blog.quantinsti.com/predictive-modeling-algorithmic-trading/
library(quantmod)
library(TTR)
library(caret)
library(corrplot)
library(pROC)
library(FSelector)
library(randomForest)
library(TTR)
library(tidyquant)
library(rpart)

## Use set.seed function to ensure the results are repeatable
set.seed(5)

## Read the stock and index data
setwd("~/git/DataScience/ML_stock selection")
df_stock = read.csv("BAJAJ-AUTO 5 Yr data.csv")
df_index = read.csv("NIFTY 5 Yr data.csv")
#
colnames(df_stock)[5] <- "Close"
colnames(df_index)[5] <- "Close"
## Compute the price change for the stock and classify as UP/DOWN
price = df_stock$Close-df_stock$Open
# What to predict? 
# 1.
class = ifelse(price > 0,"UP","DOWN")

# 2.
T.ind <- function(quotes, tgt.margin = 0.025, n.days = 10) {
  v <- apply(HLC(quotes), 1, mean)
  v[1] <- Cl(quotes)[1]
  
  r <- matrix(NA, ncol = n.days, nrow = NROW(quotes))
  for (x in 1:n.days) r[, x] <- Next(Delt(v, k = x), x)
  
  x <- apply(r, 1, function(x) sum(x[x > tgt.margin | x < -tgt.margin]))
  
  if (is.xts(quotes)) xts(x, time(quotes)) else x
}

T.ind(df_stock)


## Compute the various technical indicators that will be used 
# Force Index Indicator
forceindex = (df_stock$Close - df_stock$Open) * df_stock$Vol 
forceindex = c(NA,head(forceindex, -1))
head(forceindex)

# Buy & Sell signal Indicators (Williams R% and RSI)
WillR5  = WPR(df_stock[,c("High","Low","Close")], n = 5) ; WillR5 = c(NA,head(WillR5,-1)) ;
WillR10 = WPR(df_stock[,c("High","Low","Close")], n = 10) ; WillR10 = c(NA,head(WillR10,-1)) ;
WillR15 = WPR(df_stock[,c("High","Low","Close")], n = 15) ; WillR15 = c(NA,head(WillR15,-1)) ;
#
RSI5  = RSI(df_stock$Close, n = 5, maType="WMA")
RSI5 = c(NA,head(RSI5,-1)) 
RSI10 = RSI(df_stock$Close, n = 10,maType="WMA")
RSI10 = c(NA,head(RSI10,-1))
RSI15 = RSI(df_stock$Close, n = 15,maType="WMA")
RSI15 = c(NA,head(RSI15,-1))

# Price change Indicators (ROC and Momentum)
ROC5 = ROC(df_stock$Close, n = 5,type ="discrete")*100
ROC5 = c(NA,head(ROC5,-1)) 
ROC10 = ROC(df_stock$Close, n = 10,type ="discrete")*100
ROC10 = c(NA,head(ROC10,-1))
#
MOM5 = momentum(df_stock$Close, n = 5, na.pad = TRUE)
MOM5 = c(NA,head(MOM5,-1))
MOM10 = momentum(df_stock$Close, n = 10, na.pad = TRUE)
MOM10 = c(NA,head(MOM10,-1))
#
MOM5Indx = momentum(df_index$Close, n = 5, na.pad = TRUE)
MOM5Indx = c(NA, head(MOM5Indx, -1))
MOM10Indx = momentum(df_index$Close, n = 10, na.pad = TRUE)
MOM10Indx = c(NA, head(MOM10Indx,-1))

# Volatility signal Indicator (ATR)
ATR5 = ATR(df_stock[, c("High","Low","Close")], n = 5, maType="WMA")[,1] 
ATR5 = c(NA, head(ATR5,-1))
ATR10 = ATR(df_stock[, c("High","Low","Close")], n = 10, maType="WMA")[,1]
ATR10 = c(NA, head(ATR10,-1))
#
ATR5Indx = ATR(df_index[,c("High","Low","Close")], n = 5, maType="WMA")[,1]
ATR5Indx = c(NA,head(ATR5Indx,-1)) 
ATR10Indx = ATR(df_index[,c("High","Low","Close")], n = 10, maType="WMA")[,1]
ATR10Indx = c(NA,head(ATR10Indx,-1))
#
myMACD  <- MACD(df_stock$Close)[,2]
myVolat <- volatility(df_stock$Close,calc="close")
mySD <- runSD(df_stock$Close)
myMean <- runMean(df_stock$Close)

## Combining all the Indicators and the Class into one dataframe
dataset = data.frame(class, forceindex, WillR5, WillR10, WillR15, RSI5, RSI10, RSI15, ROC5,
                     ROC10, MOM5, MOM10, ATR5, ATR10, MOM5Indx, MOM10Indx, ATR5Indx, ATR10Indx,
                     myMACD, myVolat, mySD, myMean)
dataset = na.omit(dataset)

## Understanding the dataset using descriptive statistics
print(head(dataset),5)
dim(dataset)
y = dataset$class
cbind(freq=table(y), percentage=prop.table(table(y))*100)

summary(dataset)

##  Visualizing the dataset using a correlation matrix
correlations = cor(dataset[,c(2:18)])
print(head(correlations))
corrplot(correlations, method="circle")

## Selecting features using the random.forest.importance function from the FSelector package
set.seed(5)
weights = random.forest.importance(class~., dataset, importance.type = 1)
print(weights)
#
set.seed(5)
subset = cutoff.k(weights, 10)
print(subset)

## Creating a dataframe using the selected features
dataset_rf = data.frame(class,forceindex,WillR5,WillR10,RSI5,RSI10,RSI15,ROC5,ROC10,MOM5,MOM10Indx)
dataset_rf = na.omit(dataset_rf)

# Resampling method used - 10-fold cross validation 
# with "Accuracy" as the model evaluation metric.
trainControl = trainControl(method="cv", number=10)
metric = "Accuracy"

## Trying four different Classification algorithms
# k-Nearest Neighbors (KNN)
set.seed(5)


fit.knn = train(class~., data=dataset_rf, method="knn", 
                metric=metric, preProc=c("range"),trControl=trainControl)

# Classification and Regression Trees (CART)
set.seed(5)
fit.cart = train(class~., data=dataset_rf, method="rpart", 
                 metric=metric,preProc=c("range"),trControl=trainControl)

# Naive Bayes (NB)
set.seed(5)
fit.nb = train(class~., data=dataset_rf, method="nb", 
               metric=metric, preProc=c("range"),trControl=trainControl)

# Support Vector Machine with Radial Basis Function (SVM)
set.seed(5)
fit.svm = train(class~., data=dataset_rf, method="svmRadial", 
                metric=metric,preProc=c("range"),trControl=trainControl)

## Evaluating the algorithms using the "Accuracy" metric
results = resamples(list(KNN=fit.knn, CART=fit.cart, NB=fit.nb, SVM=fit.svm))
summary(results)
dotplot(results)

## Tuning the shortlisted algorithm (KNN algorithm)
set.seed(5)
grid = expand.grid(.k=seq(1,10,by=1))
fit.knn = train(class~., data=dataset_rf, method="knn", metric=metric, tuneGrid=grid,
                 preProc=c("range"), trControl=trainControl)
print(fit.knn)






