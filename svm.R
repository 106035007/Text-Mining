# R筆記 – (14)Support Vector Machine/Regression(支持向量機SVM)----
# https://rpubs.com/skydome20/R-Note14-SVM-SVR
library(e1071)
library(mlbench)
data(Glass, package="mlbench")
data = Glass
str(data)

smp.size = floor(0.8*nrow(data)) 
set.seed(516)                     
train.ind = sample(seq_len(nrow(data)), smp.size)
train = data[train.ind, ] # 80%
test = data[-train.ind, ] # 20%

model = svm(formula = Type ~ .,  # 依變數(在這裡是Type)的資料形態要是Factor
            data = train)

# 可以看到SVM預設的參數設定
summary(model)

# 預測
train.pred = predict(model, train)
test.pred = predict(model, test)

# 訓練資料的混淆矩陣
table(real=train$Type, predict=train.pred)

# 訓練資料的分類準確率
confus.matrix = table(real=train$Type, predict=train.pred)
sum(diag(confus.matrix))/sum(confus.matrix)

# 測試資料的混淆矩陣
table(real=test$Type, predict=test.pred)
# 測試資料的分類準確率
confus.matrix = table(real=test$Type, predict=test.pred)
sum(diag(confus.matrix))/sum(confus.matrix)

# Support Vector Regression(SVR)----
data = data.frame(x=1:20,
                  y=c(3,4,8,2,6,10,12,13,15,14,17,18,20,17,21,22,25,30,29,31))

# 資料的原始值
plot(data$x, data$y, pch=16, xlab="X", ylab="Y")

model <- lm(y ~ x , data) 

# lm預測
lm.pred = predict(model, data)

# 資料的原始值(黑點)
plot(data$x, data$y, pch=16, xlab="X", ylab="Y")
# lm的預測值(紅三角形)
points(lm.pred, pch=2, col="red")
abline(model, col="red")
# 我們可以直接用SVR來建模、預測
model <- svm(y ~ x , data) # 依變數的型態要是numeric

# 預測
svr.pred = predict(model, data)

# 資料的原始值(黑點)
plot(data$x, data$y, pch=16, xlab="X", ylab="Y")
# SVR的預測值(藍叉)
points(svr.pred, pch=4, col="blue")

# 最後比較一下線性迴歸和SVR的表現，同時計算RMSE(root mean square error)
# 資料的原始值(黑點)
plot(data$x, data$y, pch=16, xlab="X", ylab="Y")
# lm的預測值(紅三角形)
points(lm.pred, pch=2, col="red")
# SVR的預測值(藍叉)
points(svr.pred, pch=4, col="blue")
## (lm, SVR) in RMSE
c(sqrt(mean((data$y - lm.pred)^2)),
  sqrt(mean((data$y - svr.pred)^2))
)
# 可以發現到，在這個例子，SVR比lm的效果還要好一些些(1.79 < 1.91)


# C: parameter setup ----
#C越大，代表容錯越小，越少support vectors，越接近hard-margin SVM的概念，卻容易overfitting
#C越小，代表容錯越大，越多support vectors，可以追求更大的margin

require("mlbench")
data(Glass, package="mlbench")
data = Glass

# 記錄每一次提高C時，對應的support vectors數量
num.SV = sapply(X=1:1000, 
                FUN=function(C) svm(Type~., data, cost=C, epsilon =.1)$tot.nSV)

# 隨著C越大，support vectors數量越少，代表margin的範圍越窄，越接近hard-margin SVM，越容易overfitting
plot(x=1:1000, y=num.SV, xlab="C value", ylab="# of support vectors", pch=16, cex=.5, main="# of SVs in soft-margin SVM")

# Epsilon (ε)----
# 這個參數主要影響的會是SVR，而非SVM。因為在SVR的損失函數中，使用的是
# epsilon intensive hinge loss。

df = data.frame(x=1:20,
                y=c(3,4,8,2,6,10,12,13,15,14,17,18,20,17,21,22,25,30,29,31))

# 記錄每一次提高Epsilon時，對應SVR中的support vectors數量
num.SV = sapply(X=seq(0,1,0.01), 
                FUN=function(e) svm(y~x, df, cost=1, epsilon =e)$tot.nSV)
# 隨著Epsilon越大，support vectors的數量減少
plot(x=seq(0,1,0.01), y=num.SV, xlab="ε value", ylab="# of support vectors", pch=16, cex=.5, main="# of SVs in SVR")

# 記錄每一次提高Epsilon時，對應SVR中的RMSE
RMSE = sapply(X=seq(0,1,0.01), 
              FUN=function(e) sqrt(mean((svm(y~x, df, cost=1, epsilon =e)$residuals)^2)))
# 隨著Epsilon越大，模型的準確度越低，RMSE提高
plot(x=seq(0,1,0.01), y=RMSE, xlab="ε value", ylab="RMSE", pch=16, cex=.5, main="RMSE in SVR")

# Gamma----
#這是用在kernel function中的參數，主要是polynomial、radial basis(RBF)和sigmoid。
#在定義中，Gamma = How far the influence of a single training example reaches，意思是(參考)：
#gamma大，資料點的影響力範圍比較近，對超平面來說，近點的影響力權重較大，容易勾勒出擬合近點的超平面，也容易造成overfitting。
#gamma小，資料點的影響力範圍比較遠，對超平面來說，較遠的資料點也有影響力，因此能勾勒出平滑、近似直線的超平面。

require("mlbench")
data(Glass, package="mlbench")
data = Glass
smp.size = floor(0.8*nrow(data)) 
set.seed(516)                     
train.ind = sample(seq_len(nrow(data)), smp.size)
train = data[train.ind, ] # 80%
test = data[-train.ind, ] # 20%

# 記錄每一次提高gamma時，對應的訓練資料的分類準確率
train.accuracy = sapply(X=seq(0.1,10,0.1), 
                        FUN=function(g){
                          model = svm(Type~., train, gamma=g, epsilon =.1)
                          pred = predict(model, train)
                          confus.matrix = table(real=train$Type, predict=pred)
                          sum(diag(confus.matrix))/sum(confus.matrix)
                        } 
)

# 記錄每一次提高gamma時，對應的測試資料的分類準確率
test.accuracy = sapply(X=seq(0.1,10,0.1), 
                       FUN=function(g){
                         model = svm(Type~., train, gamma=g, epsilon =.1)
                         pred = predict(model, test)
                         confus.matrix = table(real=test$Type, predict=pred)
                         sum(diag(confus.matrix))/sum(confus.matrix)
                       } 
)


# Train accuracy(紅點)
plot(x=seq(0.1,10,0.1), y=train.accuracy, pch=16, cex=.5, col="red", ylim=c(0,1),xlab="gamma value", ylab="Class Accuracy", main="Accuracy in soft-margin SVM")
# Test accuracy(藍點)
points(x=seq(0.1,10,0.1), y=test.accuracy, pch=16, cex=.5, col="blue")

legend("bottomright", pch = 16, col = c("red","blue"),legend=c("Train-Accuracy", "Test-Accuracy"))

