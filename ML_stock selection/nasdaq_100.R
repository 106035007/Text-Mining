# http://amunategui.github.io/wallstreet/
library(quantmod)
getSymbols(c("AMZN"))
getSymbols('00637L.tw')

barChart(AMZN,theme='white.mono',bar.type='hlc')

getSymbols(c("^GSPC"))
chartSeries(GSPC, subset='last 3 months')
addBBands(n = 20, sd = 2, ma = "SMA", draw = 'bands', on = -1)

Nasdaq100_Symbols <- c('AAPL', 'ADBE', 'ADP', 'ADSK', 'AKAM', 'ALTR', 'AMAT', 'AMGN', 'AMZN',
'APOL', 'ATVI', 'BBBY', 'BIDU', 'BIIB', 'BRCM', 'CA', 'CECO', 'CELG',
'CERN', 'CHIR', 'CHKP', 'CHRW', 'CMCSA', 'COST', 'CSCO', 'CTAS', 'CTSH',
'CTXS', 'DELL', 'DISH', 'DTV', 'EBAY', 'ESRX', 'EXPD', 'EXPE', 'FAST',
'FFIV', 'FISV',	'FLEX',	'FLIR',	'FSLR',	'GILD',	'GOOG',	'GRMN',	'HOLX',
'HSIC',	'IACI',	'ILMN',	'INFY',	'INTC',	'INTU',	'ISIL',	'ISRG',	'KLAC',	'LBTYA',
'LLTC',	'LOGI',	'LRCX',	'MAT',	'MCHP',	'MRVL',	'MSFT',	'MU',	'MXIM',	'MYL',	
'NFLX',	'NIHD',	'NTAP',	'NVDA',	'NWSA',	'ORCL',	'ORLY',	'PAYX',	'PCAR',	'PCLN',	'QCOM',	'QGEN',
'ROST',	'SBUX',	'SHLD',	'SIRI',	'SNDK',	'SPLS',	'SRCL',	'STX',	'SYMC',	'URBN',	'VOD',	'VRSN',	
'VRTX',	'WYNN', 'XLNX',	'XRAY',	'YHOO')

#Nasdaq100_Symbols <- c("AAPL", "ADBE", "ADI", "ADP", "ADSK", "AKAM", "ALTR", "ALXN",
#                       "AMAT", "AMGN", "AMZN", "ATVI", "AVGO", "BBBY", "BIDU", "BIIB",
#                       "BRCM", "CA", "CELG", "CERN", "CHKP", "CHRW", "CHTR", "CMCSA",
#                       "COST", "CSCO", "CTRX", "CTSH", "CTXS", "DISCA", "DISCK", "DISH",
#                       "DLTR", "DTV", "EBAY", "EQIX", "ESRX", "EXPD", "EXPE", "FAST",
#                       "FB", "FFIV", "FISV", "FOXA", "GILD", "GMCR", "GOOG", "GOOGL",
#                       "GRMN", "HSIC", "ILMN", "INTC", "INTU", "ISRG", "KLAC", "KRFT",
#                       "LBTYA", "LLTC", "LMCA", "LMCK", "LVNTA", "MAR", "MAT", "MDLZ",
#                       "MNST", "MSFT", "MU", "MXIM", "MYL", "NFLX", "NTAP", "NVDA",
#                       "NXPI", "ORLY", "PAYX", "PCAR", "PCLN", "QCOM", "QVCA", "REGN",
#                       "ROST", "SBAC", "SBUX", "SIAL", "SIRI", "SNDK", "SPLS", "SRCL",
#                       "STX", "SYMC", "TRIP", "TSCO", "TSLA", "TXN", "VIAB", "VIP",
#                       "VOD", "VRSK", "VRTX", "WDC", "WFM", "WYNN", "XLNX", "YHOO")
getSymbols(Nasdaq100_Symbols)
# delete tickers without data (only 79 stocks left)
nasdaq100 <- data.frame(as.xts(merge(AAPL, ADBE, ADP, ADSK, AKAM,
                                     ALTR, AMAT, AMGN, AMZN, ATVI, BBBY, BIDU, BIIB,
                                     CA, CELG, CERN, CHIR, CHKP, CHRW, CMCSA,
                                     COST, CSCO, CTAS, CTSH, CTXS, DELL,
                                     DISH, EBAY, ESRX, EXPD, EXPE, FAST,
                                     FFIV, FISV, FLEX, FSLR, GILD, GOOG, 
                                     GRMN, HOLX, HSIC, ILMN, INTC, INTU, ISRG, KLAC, 
                                     LBTYA, LOGI, LRCX, 
                                     MAT, MCHP, MRVL, MSFT, MU, MXIM, MYL, NFLX, NIHD, NTAP, NVDA,
                                     NWSA, ORCL,ORLY, PAYX, PCAR, QCOM, QGEN, 
                                     ROST, SBUX, SIRI, SRCL,
                                     STX, URBN, VOD,
                                     VRSN, VRTX, WYNN, XLNX, XRAY)))


head(nasdaq100[,1:12],2)

outcomeSymbol <- 'FISV.Volume'

library(xts)
library(ggplot2)
nasdaq100 <- xts(nasdaq100, order.by=as.Date(rownames(nasdaq100)))
nasdaq100 <- fortify(merge(nasdaq100, lm1=lag(nasdaq100[, outcomeSymbol], -1)))
names(nasdaq100)[1] <- 'date'
#
nasdaq100$outcome <- ifelse(nasdaq100[, paste0(outcomeSymbol,'.1')] > nasdaq100[,outcomeSymbol], 1, 0)
head(nasdaq100[, c('FISV.Volume', 'FISV.Volume.1', 'outcome')])
# remove shifted down volume field as we don't care by the value
nasdaq100 <- nasdaq100[,!names(nasdaq100) %in% c(paste0(outcomeSymbol,'.1'))]
#
# objDF <- nasdaq100
GetDiffDays <- function(objDF, days=c(10), offLimitsSymbols=c('outcome'), roundByScaler=3) {
  # needs to be sorted by date in decreasing order
  ind <- sapply(objDF, is.numeric)
  for (sym in names(objDF)[ind]) {
     # sym <- 'AAPL.Open'
    if (!sym %in% offLimitsSymbols) {
      print(paste('*********', sym))
      objDF[,sym] <- round(scale(objDF[,sym]),roundByScaler)
      
      print(paste('theColName', sym))
      # day <- days
      for (day in days) {
        objDF[paste0(sym,'_',day)] <- c(diff(objDF[,sym],lag = day), rep(x=0,day)) * -1
      }
    }
  }
  return (objDF)
}


nasdaq100 <- GetDiffDays(nasdaq100, days=c(1,2,3,4,5,10,20), offLimitsSymbols=c('outcome'), roundByScaler=2)

nasdaq100 <- nasdaq100[2:nrow(nasdaq100),]

dput(names(nasdaq100)[grepl('AAPL.',names(nasdaq100))])
#
nasdaq100$wday <- as.POSIXlt(nasdaq100$date)$wday
nasdaq100$yday <- as.POSIXlt(nasdaq100$date)$mday
nasdaq100$mon <- as.POSIXlt(nasdaq100$date)$mon

nasdaq100 <- subset(nasdaq100, select=-c(date))
nasdaq100 <- nasdaq100[sample(nrow(nasdaq100)),]

library(xgboost)
predictorNames <- names(nasdaq100)[names(nasdaq100) != 'outcome']

set.seed(1234)
split <- sample(nrow(nasdaq100), floor(0.7*nrow(nasdaq100)))
train <-nasdaq100[split,]
test <- nasdaq100[-split,]

bst <- xgboost(data = as.matrix(train[,predictorNames]),
               label = train$outcome,
               verbose=0,
               eta = 0.1,
               gamma = 50,
               nround = 50,
               missing = NaN,
               colsample_bytree = 0.1,
               subsample = 8.6,
               objective="binary:logistic")

predictions <- predict(bst, as.matrix(test[,predictorNames]), missing = NaN, outputmargin=TRUE)

library(pROC)
auc <- roc(test$outcome, predictions)
print(paste('AUC score:', auc$auc))

















