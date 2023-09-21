#____Install blotter and quantstrat______
# First it is necessary install RTools 4.3 from:
# https://cran.r-project.org/bin/windows/Rtools/
#require(devtools)
#install_github("braverock/blotter")
#library(remotes)
#remotes::install_github("braverock/blotter")
#library(devtools)
#install_local(path = "blotter-master.zip")
#devtools::install_github("braverock/quantstrat")

#___Packages___
library(devtools)
library(blotter)
library(quantstrat)
library(quantmod)
library(FinancialInstrument)
library(PerformanceAnalytics)
library(foreach)
library(tidyverse) 
library(dplyr) 
library(TTR)
library("IRdisplay")
library(highcharter) #Interactive Plot
library(devtools)
library(yahoofinancer)

#rm(list = ls(all.names = T))
.blotter <- new.env() #create the required environments
.strategy <- new.env() #create the required environments

# ___Inputs___
initDate <- "2000-01-02" #Date of initiation
from <- "2016-06-19"
to <- "2023-09-20"
orderqty <- 1 # units stocks   orderqty = tradeSize / ClosePrice
initEq <- 100 #Initial equity USD
tradesize <- 276 #USD amount you wager on each trade 276

symbols <- "MSFT"
getSymbols(symbols, from = from, to = to, adjust = TRUE)
datos <- MSFT

#set the currency and the stock we are interested
currency("USD")
Sys.setenv(TZ="UTC") #setting up the timezone
stock(symbols, currency="USD", multiplier=1)

#-------------Initiate portfolio and account-----------
strategy.st <- "strat2" #Name the strategy
account.st <- "strat2"
portfolio.st <- "strat2"
rm.strat(strategy.st)

#Initiate portfolio
initPortf(name=portfolio.st, 
          symbols=symbols, 
          initDate=initDate,
          currency = "USD"
)

#Initiate account
initAcct(name=account.st, 
         portfolios=portfolio.st,    
         initDate=initDate, 
         initEq=initEq)

#Initiate orders
initOrders(portfolio=portfolio.st, 
           initDate=initDate
)

#Store all the events in the strategy
strategy(strategy.st, store=TRUE)

### -------Add Indicators------
# Add indicator to buy signal simple moving average (SMA) + volatility
RB <- function(x,n,s,l){
  sd <- runSD(x, n, sample= FALSE) # Determine the running standard deviation
  med <- runMedian(sd,n)
  mavg <- SMA(x,n)
  savg <- SMA(x,s)
  lavg <- SMA(x,l)
  signal <- ifelse(savg > lavg & sd < med,1,0)
  colnames(signal) <- "RB"
  reclass(signal,x) #to convert it into an xts object
}

nfast <- 12
nslow <- 26
n <- 35

add.indicator(strategy.st, 
              name="RB",
              arguments=list(
                x=quote(Cl(mktdata)), 
                n=n, s=nfast, l=nslow),
              label="RB")


# Add fast SMA indicator
add.indicator(strategy.st, 
              name = "SMA",
              arguments = list(
                x = quote(Cl(mktdata)),
                n = nfast
              ),
              label="nFast"
)
# Add slow SMA indicator
add.indicator(strategy.st, 
              name="SMA",
              arguments = list(
                x = quote(Cl(mktdata)),
                n = nslow
              ),
              label="nSlow"
)

# Use applyIndicators to test out your indicators
prueba <- applyIndicators(strategy = strategy.st, mktdata = HLC(datos))

##____Add Signals___
# Sell signal if SMA30<SMA200
add.signal(strategy.st, 
           name="sigComparison",
           arguments=list(
             columns=c("nFast","nSlow"), 
             relationship="lt"), 
           label="sell")

# Buy signal if RB>=1
add.signal(strategy.st, 
           name="sigThreshold", 
           arguments = list(threshold=1, column="RB",
                            relationship="gte",
                            cross=TRUE),
           label="buy")



prueba_init <- applyIndicators(strategy.st, mktdata = OHLC(datos))
prueba2 <- applySignals(strategy = strategy.st, mktdata = prueba_init)


###----RULES----
# Buy Rule
add.rule(strategy.st, 
         name='ruleSignal', 
         arguments = list(sigcol="buy", 
                          sigval=TRUE,  
                          orderqty=orderqty, 
                          ordertype='market', 
                          orderside='long', 
                          pricemethod='market', 
                          replace=FALSE), 
         type='enter', 
         path.dep=TRUE)

# Sell Rule
add.rule(strategy.st, 
         name='ruleSignal', 
         arguments = list(sigcol="sell", 
                          sigval=TRUE,  
                          orderqty='all', 
                          ordertype='market', 
                          orderside='long', 
                          pricemethod='market', 
                          replace=FALSE), 
         type='exit', 
         path.dep=TRUE) 

out <- applyStrategy(strategy.st, portfolio.st)


# Update
# Update your portfolio (portfolio.st)
updatePortf(portfolio.st)

# Update your account (account.st)
daterange <- time(getPortfolio(portfolio.st)$summary)[-1]
daterange
updateAcct(account.st, daterange)

updateEndEq(account.st)

# ------Profit Factor------
# A profit factor above 1 means your strategy is profitable.
# A profit factor below 1 means you should head back to the drawing board
# Get the tradeStats for your portfolio
tstats <- tradeStats(Portfolios = portfolio.st)

# Print the profit factor
tstats$Profit.Factor
# This percent positive statistic means that approximately 71% of your 
#trades returned a positive result.
tstats$Percent.Positive 

# plot
for(symbol in symbols) {
  chart.Posn(Portfolio=portfolio.st,
             Symbol=symbol,
             log=TRUE)
}

# summary of strategy object
summary(getStrategy(portfolio.st))
# summary of strategy object
summary(getStrategy(portfolio.st))
# join the trades to mktdata
trades <- getTxns(portfolio.st, symbols)
trades <- data.frame(trades)
prices <- cbind(mktdata, trades)
prices <- prices[-1,]
prices.sub <- prices[,c(1:6, 12)]
# process to replace NA´s with the Last Non-Missing Value
prices.sub <- data.frame(prices.sub)
prices.sub$Txn.Qty[is.na(prices.sub$Txn.Qty)] <- 0
#prices.sub <- prices.sub |> fill(colnames(prices.sub),.direction = "downup")
# transform to xts zoo form
prices.sub <- as.xts(prices.sub)
colnames(prices.sub) <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted', 'Txn.Qty')
head(prices.sub)

# Cash Sharpe ratio
portpl <- .blotter$portfolio.strat2$summary$Net.Trading.PL
SharpeRatio.annualized(portpl, geometric = FALSE)

# Returns Sharpe ratio 
# Get instrument returns
instrets <- PortfReturns(portfolio.st)
# Compute Sharpe ratio from returns
SharpeRatio.annualized(instrets, geometric = FALSE)


# evaluate the performance to see how is the return of the trading strategy
rets <- PortfReturns(Account = account.st)
rownames(rets) <- NULL
tab <- table.Arbitrary(rets,
                       metrics=c(
                         "Return.cumulative",
                         "Return.annualized",
                         "SharpeRatio.annualized",
                         "CalmarRatio"),
                       metricsNames=c(
                         "Cumulative Return",
                         "Annualized Return",
                         "Annualized Sharpe Ratio",
                         "Calmar Ratio"))
tab

charts.PerformanceSummary(rets, colorset = "#DB005B")

#Interactive Plot
highchart(type="stock") |> 
  hc_yAxis_multiples(list(title = list(text = "Price"), opposite = FALSE),
                     list(showLastLabel = FALSE, opposite = TRUE, title = list(text = "Signal"))) |>
  hc_add_series(prices.sub[,1:6]) |> 
  hc_add_series(SMA((prices.sub$Close),n=12),name="SMA(12)") |> 
  hc_add_series(SMA((prices.sub$Close),n=26),name="SMA(26)") |>
  hc_add_series(prices.sub$Txn.Qty, name="Estrategia", yAxis = 1, color = "#F24C3D") |>
  hc_title(text=paste0("<b>Prices company: ", symbols[1], "</b>")) |>
  hc_add_theme(hc_theme_darkunica())


