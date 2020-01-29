# A Brief Introduction to Quantitative Trading and the Quanstrat Library
# https://statsmaths.github.io/stat395-f17/assets/final_project/amarnani.html
# https://ntguardian.wordpress.com/2017/05/22/the-end-of-the-honeymoon-falling-out-of-love-with-quantstrat/

if (!require("TTR")) {
  install.packages("TTR")
  library(TTR)
}
if (!require("quantstrat")) {
  if(!require("devtools")) {
    install.packages("devtools")
    require(devtools)
  }
  install_github("braverock/blotter") # dependency
  install_github("braverock/quantstrat")
}

if (!require("IKTrading")){
  install_github("IlyaKipnis/IKTrading", force=TRUE)
}
#
library(devtools)
library(quantmod)
library(quantstrat)
library(TTR)
library(png)
library(IKTrading)

#
rm(list=ls())
rm(list = ls(.blotter), envir = .blotter)

initdate <- "2010-01-01"
from <- "2011-01-01" #start of backtest
to <- "2017-01-01" #end of backtest

Sys.setenv(TZ= "EST") #Set up environment for timestamps

currency("USD") #Set up environment for currency to be used

symbols <- c("AAPL", "MSFT", "GOOG", "AMZN", "IBM") #symbols used in our backtest
getSymbols(Symbols = symbols, from=from, to=to) #receive data from google finance,  adjusted for splits/dividends
#
# tradesize <-10000 #default trade size
initEq <- 100000 #default initial equity in our portfolio

.blotter <- new.env()
# 
strategy.st <- "firststrat" 
portfolio.st <- "firststrat"
account.st <- "firststrat"
#removes old portfolio and strategy from environment
rm.strat(portfolio.st)
rm.strat(account.st)
rm.strat(strategy.st) 

# initialize portfolio, account, orders and strategy objects
# This creates a portfolio
initPortf(portfolio.st, symbols = symbols, initDate = initdate, currency = "USD")

# Then an account
initAcct(account.st, portfolios = portfolio.st, initDate = initdate, currency = "USD", initEq = initEq)

# And finally an orderbook
initOrders(portfolio.st, initDate = initdate)
# This is how to create a strategy
strategy(strategy.st, store=TRUE)

#Plots the 50, 200 day SMA
candleChart(IBM, up.col = "black", dn.col = "red", theme = "white")

addSMA(n = c(200,50), on = 1, col = c("red", "blue"))

#Plots the RSI with lookback equal to 10 days 
plot(RSI(Cl(AMZN), n=10))

#------------------------------------------------------------------------------------
# Step 2: Adding indicators----
# Add indicators
add.indicator(strategy = strategy.st,
              name = 'SMA',
              arguments = list(x = quote(Cl(mktdata)), n=200),
              label = 'SMA200')
# To check if the indicator is defined correctly, use try(applyIndicators) to see if it works.
test <- try(applyIndicators(strategy.st, mktdata = OHLC(AAPL)))
head(test, 201)

#
add.indicator(strategy = strategy.st,
              name = 'RSI',
              arguments = list(x = quote(Cl(mktdata)), n=3),
              label = 'RSI_3')
#-------------------------------------------------------------------------------------
# Step 3: Generating trading signals ----
# Trading signals are generated from the trading indicators.
# First Signal: sigComparison specifying when 50-day SMA above 200-day SMA
add.signal(strategy.st, name = 'sigComparison',
           arguments = list(columns=c("SMA50", "SMA200")),
           relationship = "gt",
           label = "longfilter")

#Second Signal: sigCrossover specifying the first instance when 50-day SMA below 200-day SMA 
add.signal(strategy.st, name = "sigCrossover",
           arguments = list(columns=c("SMA50", "SMA200")),
           relationship = "lt",
           lablel = "sigCrossover.sig")

#Third Signal: sigThreshold which specifies all instance when RSI is below 20 (indication of asset being oversold)
add.signal(strategy.st, name = "sigThreshold",
           arguments = list(column = "RSI_3", threshold = 20,
                            relationship = "lt", cross = FALSE),
           label = "longthreshold")

#Fourth Signal: sigThreshold which specifies the first instance when RSI is above 80 (indication of asset being overbought)
add.signal(strategy.st, name = "sigThreshold",
           arguments = list(column = "RSI_3", threshold = 80,
                            relationship = "gt", cross = TRUE),
           label = "thresholdexit")

#Fifth Signal: sigFormula which indicates that both longfilter and longthreshold must be true.
add.signal(strategy.st, name = "sigFormula",
           arguments = list(formula = "longfilter & longthreshold",
                            cross = TRUE),
           label = "longentry")

#--------------------------------------------------------------------------------------------
# Step 4: Creating trading rules ----
# When trading signals tell us buy or sell, but it does not specify the execution details.
# The first rule will be an exit rule. This exit rule will execute when the market environment
# is no longer conducive to a trade (i.e. when the SMA-50 falls below SMA-200)
add.rule(strategy.st, 
         name = 'ruleSignal', 
         arguments = list(sigcol = "sigCrossover.sig", 
                          sigval = TRUE, 
                          orderqty = 1000, 
                          ordertype = 'market', 
                          orderside = 'long', 
                          pricemethod = 'market', 
                          replace = FALSE), 
         type = 'exit', 
         path.dep = TRUE)
# add.rule(strategy.st, 
#          name = "ruleSignal",
#          arguments = list(sigcol = "sigCrossover.sig", 
#                           sigval = TRUE,
#                           orderqty = 1000, 
#                           ordertype = "market",
#                           orderside = "long", 
#                           replace = FALSE),
#          type = "exit")
#The second rule, similar to the first, executes when the RSI has crossed above 80. 
add.rule(strategy.st, 
         name = "ruleSignal",
         arguments = list(sigcol = "thresholdexit", 
                          sigval = TRUE,
                          orderqty = 1000, 
                          ordertype = "market",
                          orderside = "long", 
                          pricemethod = 'market',
                          replace = FALSE),
         type = "exit", 
         path.dep = TRUE)

#Additionally, we also need an entry rule. This rule executes when longentry is true (or when long filter and longthreshold are true). That is when SMA-50 is above SMA-200 and the RSI is below 20.
add.rule(strategy.st, 
         name = "ruleSignal",
         arguments = list(sigcol = "longentry", 
                          sigval = TRUE,
                          orderqty = 1000, 
                          ordertype = "market",
                          orderside = "long", 
                          replace = FALSE,
                          prefer = "Open", 
                          osFUN = IKTrading::osMaxDollar,
                          tradeSize = tradesize, 
                          maxSize = tradesize),
         type = "enter")
#
# To review, the following is essentially the strategy we have coded up thus far:
#  Buy When: SMA-50 > SMA-200 AND the RSI < 20
#  Sell When: SMA-50 < SMA-200 OR RSI > 80

out <- try(applyStrategy(strategy = strategy.st, portfolios = portfolio.st))


out <- applyStrategy(strategy = strategy.st, portfolios = portfolio.st)
updatePortf(portfolio.st)
daterange <- time(getPortfolio(portfolio.st)$summary)[-1]

updateAcct(account.st, daterange)
updateEndEq(account.st)

for(symbol in symbols){
  chart.Posn(Portfolio = portfolio.st, Symbol = symbol, 
             TA= c("add_SMA(n=50, col='blue')", "add_SMA(n=200, col='red')"))
}








