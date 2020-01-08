# Data mining with R Leaning with cases
# Chapter 5 : Predicting stock returns
# https://ltorgo.github.io/DMwR2/Rstocks.html#what_to_predict

rm(list=ls())

library(DMwR2)
library(xts)
library(ggplot2)
library(grid)
library(quantmod)

data(GSPC, package="DMwR2")
first(GSPC)
last(GSPC)

library(quantmod)
library(tbl2xts)
# GSPC <- getSymbols("^GSPC",auto.assign=FALSE)
# AAPL <- read.csv("../ML_stock selection/AAPL_2007_2019.csv")
# AAPL <- as.xts(AAPL[, c(-1, -2)], order.by = as.Date(AAPL[,1], format = '%Y-%m-%d'))
# GSPC <- AAPL


GSPC <- getSymbols("^GSPC",from="1970-01-02",to="2016-01-25",auto.assign=FALSE)
class(GSPC)
dim(GSPC)
#### sub-section:  What to Predict?

##
# quotes <- GSPC
T.ind <- function(quotes, tgt.margin = 0.025, n.days = 10) {
  v <- apply(HLC(quotes), 1, mean)
  v[1] <- Cl(quotes)[1]
  
  r <- matrix(NA, ncol = n.days, nrow = NROW(quotes))
  for (x in 1:n.days) r[, x] <- Next(Delt(v, k = x), x)
  
  x <- apply(r, 1, function(x) sum(x[x > tgt.margin | x < -tgt.margin]))
  
  if (is.xts(quotes)) xts(x, time(quotes)) else x
}

candleChart(last(GSPC,'3 months'),theme='white', TA=NULL)
avgPrice <- function(p) apply(HLC(p),1,mean)
addAvgPrice <- newTA(FUN=avgPrice,col=1,legend='AvgPrice')
addT.ind <- newTA(FUN=T.ind,col='red', legend='tgtRet')
addAvgPrice(on=1) 
addT.ind()
##
avgPrice <- function(p) apply(HLC(p), 1, mean) 
addAvgPrice <- newTA(FUN=avgPrice, col=1, legend='AvgPrice')
addT.ind <- newTA(FUN=T.ind, col='red', legend='tgtRet')
candleChart(last(GSPC,'3 months'), theme='white', TA=c(addAvgPrice(on=1), addT.ind()))
#


#### sub-section:  Which Predictors?

##
library(TTR)
myATR        <- function(x) ATR(HLC(x))[,'atr']
mySMI        <- function(x) SMI(HLC(x))[, "SMI"]
myADX        <- function(x) ADX(HLC(x))[,'ADX']
myAroon      <- function(x) aroon(cbind(Hi(x),Lo(x)))$oscillator
myBB         <- function(x) BBands(HLC(x))[, "pctB"]
myChaikinVol <- function(x) Delt(chaikinVolatility(cbind(Hi(x),Lo(x))))[, 1]
myCLV        <- function(x) EMA(CLV(HLC(x)))[, 1]
myEMV        <- function(x) EMV(cbind(Hi(x),Lo(x)),Vo(x))[,2]
myMACD       <- function(x) MACD(Cl(x))[,2]
myMFI        <- function(x) MFI(HLC(x),  Vo(x))
mySAR        <- function(x) SAR(cbind(Hi(x),Cl(x))) [,1]
myVolat      <- function(x) volatility(OHLC(x),calc="garman")[,1]

##
library(randomForest)
data.model <- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC),k=1:10) + 
                             myATR(GSPC) + mySMI(GSPC) + myADX(GSPC) + myAroon(GSPC) + 
                             myBB(GSPC)  + myChaikinVol(GSPC) + myCLV(GSPC) + 
                             CMO(Cl(GSPC)) + EMA(Delt(Cl(GSPC))) + myEMV(GSPC) + 
                             myVolat(GSPC)  + myMACD(GSPC) + myMFI(GSPC) + RSI(Cl(GSPC)) +
                             mySAR(GSPC) + runMean(Cl(GSPC)) + runSD(Cl(GSPC)))

#T.ind(GSPC)

set.seed(1234)
rf <- buildModel(data.model,method='randomForest',
                 training.per=c("1995-01-01","2005-12-30"),
                 ntree=1000, importance=TRUE)

##
ex.model <- specifyModel(T.ind(IBM) ~ Delt(Cl(IBM), k = 1:3))
data <- modelData(ex.model, data.window = c("2009-01-01",  "2009-08-10"))
m <- myFavouriteModellingTool(ex.model@model.formula, as.data.frame(data))
#
varImpPlot(rf@fitted.model, type = 1)
# 1 = mean decrease in accuracy
imp <- importance(rf@fitted.model, type = 1)
rownames(imp)[which(imp > 30)]
#[1] "myATR.GSPC"      "mySMI.GSPC"      "myADX.GSPC"      "myAroon.GSPC"    "myEMV.GSPC"      "myVolat.GSPC"    "myMACD.GSPC"     "myMFI.GSPC"     
#[9] "mySAR.GSPC"      "runMean.Cl.GSPC" "runSD.Cl.GSPC"  
#
data.model <- specifyModel(T.ind(GSPC) ~ myATR(GSPC) + mySMI(GSPC) +  myADX(GSPC) + 
                             myAroon(GSPC) + myEMV(GSPC) + myVolat(GSPC) + 
                             myMACD(GSPC) + myMFI(GSPC) + mySAR(GSPC) + 
                             runMean(Cl(GSPC)) + runSD(Cl(GSPC)))

## The regression task
Tdata.train <- as.data.frame(modelData(data.model, data.window=c('1970-01-02','2005-12-30')))
Tdata.eval <- na.omit(as.data.frame(modelData(data.model, data.window=c('2006-01-01','2016-01-25'))))
Tform <- as.formula('T.ind.GSPC ~ .')
## The classification task
buy.thr <- 0.1
sell.thr <- -0.1
Tdata.trainC <- cbind(Signal=trading.signals(Tdata.train[["T.ind.GSPC"]], buy.thr, sell.thr), Tdata.train[,-1])
Tdata.evalC <-  cbind(Signal=trading.signals(Tdata.eval[["T.ind.GSPC"]],  buy.thr,sell.thr), Tdata.eval[,-1])

TformC <- as.formula("Signal ~ .")

# The Modeling Tools
set.seed(1234)
library(nnet)
## The first column is the target variable
norm.data <- data.frame(T.ind.GSPC=Tdata.train[[1]],scale(Tdata.train[,-1]))
nn <- nnet(Tform, norm.data[1:1000, ], size = 5, decay = 0.01, 
           maxit = 1000, linout = TRUE, trace = FALSE)
preds <- predict(nn, norm.data[1001:2000, ])
#
sigs.nn <- trading.signals(preds, 0.1, -0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000, "T.ind.GSPC"], 0.1, -0.1)
sigs.PR(sigs.nn,true.sigs)
#
set.seed(1234)
library(nnet)
norm.data <- data.frame(Signal=Tdata.trainC$Signal,scale(Tdata.trainC[,-1]))
nn <- nnet(Signal ~ ., norm.data[1:1000, ], size = 10, decay = 0.01, 
           maxit = 1000, trace = FALSE)
preds <- predict(nn, norm.data[1001:2000, ], type = "class")

sigs.PR(preds, norm.data[1001:2000, 1])

#
set.seed(1234)
library(e1071)
sv <- svm(Tform, Tdata.train[1:1000, ], gamma = 0.001, cost = 100)
s.preds <- predict(sv, Tdata.train[1001:2000, ])
sigs.svm <- trading.signals(s.preds, 0.1, -0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000, "T.ind.GSPC"], 0.1, -0.1)
sigs.PR(sigs.svm, true.sigs)
#
library(kernlab)
ksv <- ksvm(Signal ~ ., Tdata.trainC[1:1000, ], C = 10)
ks.preds <- predict(ksv, Tdata.trainC[1001:2000, ])
sigs.PR(ks.preds, Tdata.trainC[1001:2000, 1])
#
library(earth)
e <- earth(Tform, Tdata.train[1:1000, ])
e.preds <- predict(e, Tdata.train[1001:2000, ])
sigs.e <- trading.signals(e.preds, 0.1, -0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000, "T.ind.GSPC"],  0.1, -0.1)
sigs.PR(sigs.e, true.sigs)

summary(e)

evimp(e, trim=FALSE)

#
policy.1 <- function(signals,market,opened.pos,money,
                     bet=0.2,hold.time=10,
                     exp.prof=0.025, max.loss= 0.05
)
{
  d <- NROW(market) # this is the ID of today
  orders <- NULL
  nOs <- NROW(opened.pos)
  # nothing to do!
  if (!nOs && signals[d] == 'h') return(orders)
  
  # First lets check if we can open new positions
  # i) long positions
  if (signals[d] == 'b' && !nOs) {
    quant <- round(bet*money/Cl(market)[d],0)
    if (quant > 0) 
      orders <- rbind(orders,
                      data.frame(order=c(1,-1,-1),order.type=c(1,2,3), 
                                 val = c(quant,
                                         Cl(market)[d]*(1+exp.prof),
                                         Cl(market)[d]*(1-max.loss)
                                 ),
                                 action = c('open','close','close'),
                                 posID = c(NA,NA,NA)
                      )
      )
    
    # ii) short positions  
  } else if (signals[d] == 's' && !nOs) {
    # this is the nr of stocks we already need to buy 
    # because of currently opened short positions
    need2buy <- sum(opened.pos[opened.pos[,'pos.type']==-1,
                               "N.stocks"])*Cl(market)[d]
    quant <- round(bet*(money-need2buy)/Cl(market)[d],0)
    if (quant > 0)
      orders <- rbind(orders,
                      data.frame(order=c(-1,1,1),order.type=c(1,2,3), 
                                 val = c(quant,
                                         Cl(market)[d]*(1-exp.prof),
                                         Cl(market)[d]*(1+max.loss)
                                 ),
                                 action = c('open','close','close'),
                                 posID = c(NA,NA,NA)
                      )
      )
  }
  
  # Now lets check if we need to close positions
  # because their holding time is over
  if (nOs) 
    for(i in 1:nOs) {
      if (d - opened.pos[i,'Odate'] >= hold.time)
        orders <- rbind(orders,
                        data.frame(order=-opened.pos[i,'pos.type'],
                                   order.type=1,
                                   val = NA,
                                   action = 'close',
                                   posID = rownames(opened.pos)[i]
                        )
        )
    }
  
  orders
}

#
policy.2 <- function(signals,market,opened.pos,money,
                     bet=0.2,exp.prof=0.025, max.loss= 0.05
)
{
  d <- NROW(market) # this is the ID of today
  orders <- NULL
  nOs <- NROW(opened.pos)
  # nothing to do!
  if (!nOs && signals[d] == 'h') return(orders)
  
  # First lets check if we can open new positions
  # i) long positions
  if (signals[d] == 'b') {
    quant <- round(bet*money/Cl(market)[d],0)
    if (quant > 0) 
      orders <- rbind(orders,
                      data.frame(order=c(1,-1,-1),order.type=c(1,2,3), 
                                 val = c(quant,
                                         Cl(market)[d]*(1+exp.prof),
                                         Cl(market)[d]*(1-max.loss)
                                 ),
                                 action = c('open','close','close'),
                                 posID = c(NA,NA,NA)
                      )
      )
    
    # ii) short positions  
  } else if (signals[d] == 's') {
    # this is the money already committed to buy stocks
    # because of currently opened short positions
    need2buy <- sum(opened.pos[opened.pos[,'pos.type']==-1,
                               "N.stocks"])*Cl(market)[d]
    quant <- round(bet*(money-need2buy)/Cl(market)[d],0)
    if (quant > 0)
      orders <- rbind(orders,
                      data.frame(order=c(-1,1,1),order.type=c(1,2,3), 
                                 val = c(quant,
                                         Cl(market)[d]*(1-exp.prof),
                                         Cl(market)[d]*(1+max.loss)
                                 ),
                                 action = c('open','close','close'),
                                 posID = c(NA,NA,NA)
                      )
      )
  }
  
  orders
}

## Train and test periods
start <- 1
len.tr <- 1000
len.ts <- 500
tr <- start:(start+len.tr-1)
ts <- (start+len.tr):(start+len.tr+len.ts-1)
## getting the quotes for the testing period
data(GSPC)
date <- rownames(Tdata.train[start+len.tr,])
marketTP <- GSPC[paste(date,'/',sep='')][1:len.ts]
## learning the model and obtaining its signal predictions for the test period
library(e1071)
s <- svm(Tform, Tdata.train[tr,], cost=10, gamma=0.01)
p <- predict(s, Tdata.train[ts,])
sig <- trading.signals(p, 0.1, -0.1)
## now using the simulated trader during the testing period
#=================================================================================
# The following code is testing the function policy.1() 
#=================================================================================

#
signals <- sig
market <- marketTP
policy.1 <- function(signals, market, opened.pos, money,
                     bet=0.2,hold.time=10,
                     exp.prof=0.025, max.loss= 0.05
)
{
  d <- NROW(market) # this is the ID of today
  orders <- NULL
  nOs <- NROW(opened.pos)
  # nothing to do!
  if (!nOs && signals[d] == 'h') return(orders)
  
  # First lets check if we can open new positions
  # i) long positions
  if (signals[d] == 'b' && !nOs) {
    quant <- round(bet*money/Cl(market)[d],0)
    if (quant > 0) 
      orders <- rbind(orders,
                      data.frame(order=c(1,-1,-1),order.type=c(1,2,3), 
                                 val = c(quant,
                                         Cl(market)[d]*(1+exp.prof),
                                         Cl(market)[d]*(1-max.loss)
                                 ),
                                 action = c('open','close','close'),
                                 posID = c(NA,NA,NA)
                      )
      )
    
    # ii) short positions  
  } else if (signals[d] == 's' && !nOs) {
    # this is the nr of stocks we already need to buy 
    # because of currently opened short positions
    need2buy <- sum(opened.pos[opened.pos[,'pos.type']==-1,
                               "N.stocks"])*Cl(market)[d]
    quant <- round(bet*(money-need2buy)/Cl(market)[d],0)
    if (quant > 0)
      orders <- rbind(orders,
                      data.frame(order=c(-1,1,1),order.type=c(1,2,3), 
                                 val = c(quant,
                                         Cl(market)[d]*(1-exp.prof),
                                         Cl(market)[d]*(1+max.loss)
                                 ),
                                 action = c('open','close','close'),
                                 posID = c(NA,NA,NA)
                      )
      )
  }
  
  # Now lets check if we need to close positions
  # because their holding time is over
  if (nOs) 
    for(i in 1:nOs) {
      if (d - opened.pos[i,'Odate'] >= hold.time)
        orders <- rbind(orders,
                        data.frame(order=-opened.pos[i,'pos.type'],
                                   order.type=1,
                                   val = NA,
                                   action = 'close',
                                   posID = rownames(opened.pos)[i]
                        )
        )
    }
  
  orders
}










t1 <- trading.simulator(marketTP, signals=sig, policy.func='policy.1',
                        policy.pars=list(exp.prof=0.05,bet=0.2,hold.time=30))
t1
summary(t1)
str(t1)
t1@trading
t1@positions
#
tradingEvaluation(t1)

plot(t1, marketTP,  theme = "white",  name = "SP500")
#
t2 <- trading.simulator(marketTP, sig, "policy.2", list(exp.prof = 0.05, bet = 0.3))
summary(t2)
tradingEvaluation(t2)

plot(t2, marketTP,  theme = "white",  name = "SP500")




