# https://kochiuyu.github.io/r-programming/ta-with-r-ch7/
# Applied Financial Economics -- programming Excel, VBA and R , Chiu Yu Ko.
library(quantmod)

getSymbols("AAPL")

head(AAPL)

Open <- Op(AAPL)   #Open Price
High <- Hi(AAPL)    # High price
Low <- Lo(AAPL)  # Low price
Close<- Cl(AAPL)   #Close Price
Volume <- Vo(AAPL)   #Volume
AdjClose <- Ad(AAPL) # Adjusted close

getSymbols("AAPL", from='2000-01-01',to='2019-12-31')

WeekVoYa<- apply.weekly(Vo(AAPL),sum)
# sum from Monday to Friday
MonthVoYa <- apply.monthly(Vo(AAPL),sum)
# sum to month
QuarterVoYa <- apply.quarterly(Vo(AAPL),sum)
# sum to quarter
YearVoYa <- apply.yearly(Vo(AAPL),sum)
# sum to year

WeekAveVoClYa<- apply.weekly(Vo(AAPL),mean)

chartSeries(AAPL,
            type="line",
            subset='2007',
            theme=chartTheme('white'))

chartSeries(AAPL,
            type="bar",
            subset='2007-05::2007-06',
            theme=chartTheme('white'))

chartSeries(AAPL,
            type="candlesticks",
            subset='2007-05',
            theme=chartTheme('white'))


chartSeries(AAPL,
            type="auto",
            subset='2007-05-10::2007-05-30',
            theme=chartTheme('white'))

# install.packages("TTR")
library(TTR)
sma <-SMA(Cl(AAPL),n=20)
tail(sma,n=5)
#
ema <-EMA(Cl(AAPL),n=20)
tail(ema,n=5)

bb <-BBands(Cl(AAPL),s.d=2)
tail(bb,n=5)
# Momentum 
M <- momentum(Cl(AAPL), n=2)
head (M,n=5)
# 
ROC <- ROC(Cl(AAPL),n=2)
# 2-day ROC
head(ROC,n=5)
#
macd <- MACD(Cl(AAPL), nFast=12, nSlow=26,
             nSig=9, maType=SMA)
tail(macd,n=5)
#
rsi = RSI(Cl(AAPL), n=14)
tail(rsi,n=5)

chartSeries(AAPL,
            subset='2007-05::2009-01',
            theme=chartTheme('white'))

addSMA(n=30,on=1,col = "blue")
addSMA(n=200,on=1,col = "red")

chartSeries(AAPL,
            subset='2007-05::2009-01',
            theme=chartTheme('white'))
addEMA(n=30,on=1,col = "blue")

addEMA(n=200,on=1,col = "red")

chartSeries(AAPL,
            subset='2007-05::2009-01',
            theme=chartTheme('white'))
addBBands(n=20,sd=2)

chartSeries(AAPL,
            subset='2007-05::2009-01',
            theme=chartTheme('white'))
addMomentum(n=1)

chartSeries(AAPL,
            subset='2007-05::2008-01',
            theme=chartTheme('white'))
addROC(n=7)

chartSeries(AAPL,
            subset='2007-05::2008-01',
            theme=chartTheme('white'))
addMACD(fast=12,slow=26,signal=9,type="EMA")

chartSeries(AAPL,
            subset='2007-05::2009-01',
            theme=chartTheme('white'))
addRSI(n=14,maType="EMA")

chartSeries(AAPL,
            subset='2007-05::2009-01',
            theme=chartTheme('white'))
addSMA(n=30,on=1,col = "blue")

addSMA(n=200,on=1,col = "red")

NS <- function(xdat) xdat / coredata(xdat)[1]
a <- NS(Cl(AAPL))-1
g <- NS(Cl(GOOG))-1
m <- NS(Cl(MSFT))-1
chartSeries(a,
            subset = '2007',
            theme=chartTheme('white'))

#
symbols <- c("AAPL", "GOOG", "MSFT")
getSymbols(symbols)

NS <- function(xdat) xdat / coredata(xdat)[1]
a <- NS(Cl(AAPL))-1
g <- NS(Cl(GOOG))-1
m <- NS(Cl(MSFT))-1
chartSeries(a,
            subset = '2007',
            theme=chartTheme('white'))

addTA(g, on=1, col="red", lty="dotted")
addTA(m, on=1, col="blue", lty="dashed")

#
# Performance Evaluation----
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

library(quantmod)
getSymbols("MSFT")

# Strategy 1: filter (naive) ----
price <- Cl(MSFT) # close price
r <- price/Lag(price) - 1 # % price change
delta <-0.005 #threshold
signal <-c(0) # first date has no signal

#Loop over all trading days (except the first)
for (i in 2: length(price)){
  if (r[i] > delta){
    signal[i]<- 1
  } else
    signal[i]<- 0
}

# Each data is not attached with time
head(signal, n=30)

# Assign time to action variable using reclass;
signal<-reclass(signal, price)

# Each point is now attached with time
tail(signal, n=3)

# Charting with Trading rule
chartSeries(MSFT,
            type = 'line',
            subset="2009-08::2009-09-15",
            theme=chartTheme('white'))
addTA(signal,type='S',col='red')
# We consider trading based on yesterday indicator:
trade <- Lag(signal,1) # trade based on yesterday signal

ret<-dailyReturn(MSFT)*trade
names(ret)<-"filter"
head(ret, 10)

#Performance Summary
charts.PerformanceSummary(ret, main="Naive Buy Rule")
#

# 
# price <- Cl(MSFT)
# r <- price/Lag(price) - 1
# delta<-0.005
# signal <-c(NA) # first signal is NA
# #
# for (i in 2: length(Cl(MSFT))){
#   if (r[i] > delta){
#     signal[i]<- 1
#   } else if (r[i]< -delta){
#     signal[i]<- -1
#   } else
#     signal[i]<- 0
# }
# 
# signal<-reclass(signal,Cl(MSFT))
# 
# trade1 <- Lag(signal)
# ret1<-dailyReturn(MSFT)*trade1
# names(ret1) <- 'Naive'
# charts.PerformanceSummary(ret1)

# Strategy 2: RSI strategy----
#Consider following day-trading strategy based on 14-day RSI:
# buy one unit if RSI <30 and
# otherwise no trade.

day <-14
price <- Cl(MSFT)
signal <- c()                    #initialize vector
rsi <- RSI(price, day)     #rsi is the lag of RSI
signal [1:day+1] <- 0            #0 because no signal until day+1

for (i in (day+1): length(price)){
  if (rsi[i] < 30){             #buy if rsi < 30
    signal[i] <- 1
  }else {                       #no trade all if rsi > 30
    signal[i] <- 0
  }
}
signal<-reclass(signal,Cl(MSFT))
trade2 <- Lag(signal)

#construct a new variable ret1
ret1 <- dailyReturn(MSFT)*trade
names(ret1) <- 'Naive'
# construct a new variable ret2
ret2 <- dailyReturn(MSFT)*trade2
names(ret2) <- 'RSI'

retall <- cbind(ret1, ret2)
charts.PerformanceSummary(retall, 
                          main="Naive v.s. RSI")

# More efficient code
# signal [1:day+1] <- 0            
# 
# for (i in (day+1): length(price)){
#   if (rsi[i] < 30){             
#     signal[i] <- 1
#   }else {                       
#     signal[i] <- 0
#   }
# }

# Strategy 3: filter and RSI----
#Combining two indicators: filter and RSI
#Test the following strategy using filter and RSI based on day trading:
#Buy signal based on filter rule.
#Sell signal based on RSI rule.
#Tie-breaking: buy-signal has priority
#We use 14-day RSI and use 70 as threshold for selling.

n <- 14
delta<-0.005
price <- Cl(MSFT)         
r <- price/Lag(price) - 1    
rsi <- RSI(price, n) 
signal <-c()    # first signal is NA
signal[1:n] <-0

# Generate Trading Signal
for (i in (n+1):length(price)){
  if (r[i] > delta){
    signal[i]<- 1
  } else if (rsi[i] > 70){
    signal[i]<- -1
  } else
    signal[i]<- 0
}
signal<-reclass(signal,price)

## Apply Trading Rule
trade3 <- Lag(signal)
ret3<-dailyReturn(MSFT)*trade3 
names(ret3) <- 'Combine'
retall <- cbind(ret1, ret2, ret3)

charts.PerformanceSummary(
          retall, main="Naive v.s. RSI v.s. Combine",
          colorset=bluefocus)


# Trade size: day trading ----
#Wealth: 1 million
#Trade unit: 1000 stocks per trade
#Test the following strategy based on 14-day RSI :
#Buy one more unit if RSI <30.
#Keep buying the same if 30 < RSI < 50
#Stop trading if RSI >= 50
#Evaluate based on day trading
#To take trade size into account, we need to keep track of wealth:
qty <-1000
day <-14

signal <- c()    #trade signal with size
signal[1:(day+1)] <- 0 

price <- Cl(MSFT)

wealth <-c()
wealth[1:(day+1)] <- 1000000  

return<-c()                  
return[1:(day+1)] <- 0

profit <-c()
profit[1:(day+1)] <- 0

rsi <- RSI(price, day)  #rsi is the lag of RSI
for (i in (day+1): length(price)){
  if (rsi[i] < 30){  #buy one more unit if rsi < 30
    signal[i] <- signal[i-1]+1
  } else if (rsi[i] < 50){  #no change if rsi < 50
    signal[i] <- signal[i-1] 
  } else {         #sell  if rsi > 50
    signal[i] <- 0
  }
}
signal<-reclass(signal,price)
#
Close <- Cl(MSFT)
Open <- Op(MSFT)
trade <- Lag(signal)
for (i in (day+1):length(price)){
  profit[i] <- qty * trade[i] * (Close[i] - Open[i])  
  wealth[i] <- wealth[i-1] + profit[i]
  return[i] <- (wealth[i] / wealth[i-1]) -1  
}
ret3<-reclass(return,price)

charts.PerformanceSummary(ret3, main="Trade Size")

# Non-Day Trading----
#Trading signal:
#Buy signal arises if 14-day RSI < 30
#Sell signal arises if 14-day RSI > 50
#Trading Rule
#Buy 300 units under buy signal
#Sell all when sell signal appears
# Initial wealth: 10,000
#Note that we need to keep track of both cash and stock holdings.
qty <-300
day <-14

signal <- c()   #trade signal
signal[1:(day+1)] <- 0 

price <- Cl(MSFT)

stock <- c()  #stock holding
stock[1:(day+1)] <-0

cash <-c()
cash[1:(day+1)] <- 10000  

#
rsi <- RSI(price, day)  #rsi is the lag of RSI
for (i in (day+1): length(price)){
  if (rsi[i] < 30){  #buy one more unit if rsi < 30
    signal[i] <- 1
  } else if (rsi[i] < 50){ #no change if rsi < 50
    signal[i] <- 0
  } else {         #sell  if rsi > 50
    signal[i] <- -1
  }
}
signal<-reclass(signal,price)

#
trade <- Lag(signal)    #rsi is the lag of RSI
for (i in (day+1): length(price)){
  if (trade[i]>=0){
    stock[i] <- stock[i-1] + qty*trade[i]
    cash[i] <- cash[i-1] - 
      qty*trade[i]*price[i]
  } else{
    stock[i] <- 0
    cash[i] <- cash[i-1] + 
      stock[i-1]*price[i]
  }
}
stock<-reclass(stock,price)
cash<-reclass(cash,price)

#
equity <-c()
equity[1:(day+1)] <- 10000 

return<-c()                  
return[1:(day+1)] <- 0

for (i in (day+1): length(price)){
  equity[i] <- stock[i] * price[i] + cash[i]
  return[i] <- equity[i]/equity[i-1]-1
}
equity<-reclass(equity,price)
return<-reclass(return,price)

charts.PerformanceSummary(return, 
                          main="Non-Day-Trading")

chart_Series(equity, main="equity line")

chart_Series(cash, name="Cash Holding")

chart_Series(stock, name="Stock Holding")




