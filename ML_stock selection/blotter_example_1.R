# https://books.google.com.tw/books?id=GVxnDwAAQBAJ&pg=PA218&lpg=PA218&dq=getSymbols(Symbols+%3D+symbols,+src+%3D+%22google%22,+from%3Dfrom,+to%3Dto,+adjust+%3D+TRUE)&source=bl&ots=SmwaG2NQLU&sig=ACfU3U19mE9TsxsEksvCqxaRZxg5XdOadw&hl=zh-TW&sa=X&ved=2ahUKEwjY8_KEw47nAhWIHKYKHa-iCHkQ6AEwAHoECAoQAQ#v=onepage&q=getSymbols(Symbols%20%3D%20symbols%2C%20src%20%3D%20%22google%22%2C%20from%3Dfrom%2C%20to%3Dto%2C%20adjust%20%3D%20TRUE)&f=false
# Applied Financial Economics -- programming Excel, VBA and R , Chiu Yu Ko.
# https://kochiuyu.github.io/r-programming/ta-with-r-ch8/
rm(list=ls())
library(quantmod)
library(FinancialInstrument)
library(PerformanceAnalytics)
library(foreach)

library(blotter)
library(quantstrat)

options("getSymbols.warning4.0"=FALSE)
from  = "2003-01-01"
to = "2012-12-31"
symbols = c("AAPL", "IBM")
currency("USD")
getSymbols(symbols, from  = from , to = to)

stock(symbols, currency = "USD", multiplier = 1)
initEq = 1000000

#
filter <- function(price){
          lagprice <- lag(price, 1)
          temp <- lagprice/price
          colnames(temp) <- "filter"
          return(temp)
}

strategy.st <- "filter"
portfolio.st <- "filter"
account.st <- "filter"
#
rm.strat(strategy.st)
rm.strat(account.st)


initDate = "1990-01-01"

initPortf(name = portfolio.st, 
          symbols = symbols, 
          initDate = initDate, 
          currency = 'USD')
initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = initDate,
         currency = 'USD',
         initEq = initEq)
initOrders(portfolio = portfolio.st,
           symbols = symbols,
           initDate = initDate)

strategy(strategy.st, store = TRUE)

add.indicator(strategy = strategy.st,
              name = "filter", 
              arguments = list(price = quote(Cl(mktdata))), 
              label = "filter")

test <- try(applyIndicators(strategy.st, mktdata = OHLC(AAPL)))

head(test, n = 4)
# Creating Signals ----
# enter when filter > 1 + delta
add.signal(strategy.st,
           name = "sigThreshold",
           arguments = list(threshold = 1.05, 
                            column = "filter", 
                            relationship = "gt", 
                            cross = TRUE), 
           label = "filter.gt.0")
# exit when filter < 1 - delta
add.signal(strategy.st, 
           name = "sigThreshold", 
           arguments = list(threshold = 0.95, 
                            column = "filter", 
                            relationship = "lt", 
                            cross = TRUE), 
           label = "filter.lt.0")
# Creating rules ----
# Buy rule
add.rule(strategy.st, 
         name = 'ruleSignal', 
         arguments = list(sigcol = "filter.gt.0", 
                          sigval = TRUE, 
                          orderqty = 1000, 
                          ordertype = 'market', 
                          orderside = 'long', 
                          pricemethod = 'market', 
                          replace = FALSE), 
         type = 'enter', 
         path.dep = TRUE)
# Sell rule
add.rule(strategy.st, 
         name = 'ruleSignal', 
         arguments = list(sigcol = "filter.lt.0", 
                          sigval = TRUE, 
                          orderqty = -1000, 
                          ordertype = 'market', 
                          orderside = 'long', 
                          pricemethod = 'market', 
                          replace = FALSE), 
         type = 'enter', 
         path.dep = TRUE)
# Apply strategy
start_t <- Sys.time()

out <- try(applyStrategy(strategy = strategy.st, 
                         portfolios = portfolio.st))
end_t <- Sys.time()
print("Strategy Loop:")
print(end_t - start_t)

# Update portfolio
updatePortf(portfolio.st)
updateAcct(portfolio.st)
updateEndEq(account.st)

# Charting results
for (symbol in symbols) {
  chart.Posn(Portfolio = portfolio.st,
             Symbol = symbols, 
             log = TRUE)
}

# trade statistics
tstats <- tradeStats(portfolio.st)
t(tstats)

# Performance statistics
rets <- PortfReturns(Account = account.st)
rownames(rets) <- NULL
tab.perf <- table.Arbitrary(rets, 
                            metrics = c(
                              "Return.cumulative", 
                              "Return.annualized", 
                              "SharpeRatio.annualized", 
                              "CalmarRatio"), 
                            metricsNames = c(
                              "Cumulative Return", 
                              "Annualized Return", 
                              "Annualized Sharperatio", 
                              "Calmar Ratio"))
# Performance summary
charts.PerformanceSummary(rets, colorset = bluefocus)

#--------------------------------------------------------
# Buy and hold strategy for SPY ----
# Get SPY
rm.strat("buyHold")
getSymbols("SPY", from = from, to = to)
currency("USD")
stock("SPY", currency = "USD", multiplier = 1)

# Initial Setup
initPortf("buyHold", "SPY", initDate = initDate)
initAcct("buyHold", portfolios = "buyHold", 
         initDate = initDate, initEq = initEq)

# Since buy and sell are not given by trading rule, we 
# directly add transaction to it. We first add the transaction to buy at the beginning.
FirstDate <- first(time(SPY))
# Enter order on the first date
BuyDate <- FirstDate
equity  <-  getEndEq("buyHold", FirstDate)
FirstPrice <- as.numeric(Cl(SPY[BuyDate, ]))
UnitSize <- as.numeric(trunc(equity/FirstPrice))
addTxn("buyHold", Symbol = "SPY", 
       TxnDate = BuyDate, TxnPrice = FirstPrice, 
       TxnQty = UnitSize, TxnFees = 0)
# Sell last
LastDate <- last(time(SPY))
# Exit order on the last Date
LastPrice <- as.numeric(Cl(SPY[LastDate, ]))
addTxn("buyHold", Symbol = "SPY", 
       TxnDate = LastDate, TxnPrice = LastPrice, 
       TxnQty = -UnitSize, TxnFees = 0)
# Update account
updatePortf(Portfolio = "buyHold")
updateAcct(name = "buyHold")
updateEndEq(Account = "buyHold")
chart.Posn("buyHold", Symbol = "SPY")

# Buy and hold
rm("account.buyHold", pos=.blotter)
rm("portfolio.buyHold", pos=.blotter)

initPortf("buyHold", symbol=symbols)
initAcct("buyHold", portfolios = "buyHold",
         initEq = initEq)

# Compare strategy and market
rets <- PortfReturns(Account = account.st)
rets.bh <- PortfReturns(Account = "buyHold")
returns <- cbind(rets, rets.bh)
charts.PerformanceSummary(
  returns, geometric = FALSE,
  wealth.index = TRUE, 
  main = "Strategy vs. Market")


#----------------------------------------------
# Buy and hold for IBM and Apple ----
# Buy on the first day at closing price
# Sell on the last day at closing price
Apple.Buy.Date <- first(time(AAPL))
Apple.Buy.Price <- as.numeric(Cl(AAPL[Apple.Buy.Date,]))
Apple.Sell.Date <- last(time(AAPL))
Apple.Sell.Price <- as.numeric(Cl(AAPL[Apple.Sell.Date,]))
Apple.Qty <- trunc(initEq/(2*Apple.Buy.Price))

IBM.Buy.Date <- first(time(IBM))
IBM.Buy.Price <- as.numeric(Cl(IBM[IBM.Buy.Date,]))
IBM.Sell.Date <- last(time(IBM))
IBM.Sell.Price <- as.numeric(Cl(IBM[IBM.Sell.Date,]))
IBM.Qty <- trunc(initEq/(2*IBM.Buy.Price))

# We first add buy transactions to the system using the function addTxn:                            
addTxn(Portfolio = "buyHold", 
       Symbol = "AAPL", 
       TxnDate = Apple.Buy.Date, 
       TxnQty = Apple.Qty,
       TxnPrice = Apple.Buy.Price,
       TxnFees = 0)

addTxn(Portfolio = "buyHold", 
       Symbol = "IBM", 
       TxnDate = IBM.Buy.Date, 
       TxnQty = IBM.Qty,
       TxnPrice = IBM.Buy.Price,
       TxnFees = 0)
# Then we add the sell transactions:
addTxn(Portfolio = "buyHold", 
       Symbol = "AAPL", 
       TxnDate = Apple.Sell.Date, 
       TxnQty = -Apple.Qty,
       TxnPrice = Apple.Sell.Price,
       TxnFees = 0)

addTxn(Portfolio = "buyHold", 
       Symbol = "IBM", 
       TxnDate = IBM.Sell.Date, 
       TxnQty = -IBM.Qty,
       TxnPrice = IBM.Sell.Price,
       TxnFees = 0)
# Now we can update the account based on the added transactions:
updatePortf(Portfolio = "buyHold")
updateAcct(name = "buyHold")
updateEndEq(Account = "buyHold")

chart.Posn("buyHold", Symbol = "AAPL")


out <- perTradeStats("buyHold", "AAPL")
t(out)







