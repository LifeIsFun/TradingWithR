# 50 / 200 SMA cross over trategy with R 
# short and long
# based on Data Camp taught by Iiya Kipnis
# created by Heather Dye

##### INSTALL PACKAGES #################

install.packages("remotes")
remotes::install_github("braverock/quantstrat")
# if difficulties with instalation, check if stdlib.h is found, if not try one of the following:
  # unstalled through terminal: xcode-select --install
  # xcode / Preferences / components / iOS simulator
  # sudo installer -pkg /Library/Developer/CommandLineTools/Packages/macOS_SDK_headers_for_macOS_10.14.pkg -target /
# and then:
install.packages("devtools")
require(devtools)
install_github("braverock/blotter") # dependency
install_github("braverock/quantstrat")
library(quantstrat)

install.packages("quantmod")
library(quantmod)

# indicators
install.packages("TTR")
library(TTR)

install_github("IlyaKipnis/IKTrading", force=TRUE)
library(IKTrading)

install.packages("xts")
library(xts)


############################# DEFINE VARIABLES ##############################

sym           <- 'LQD'
st.1          <- 'SMACross'
port          <- 'sun'
acct          <- 'heather'


initdate      = "1999-01-01"      # initialization date
from          = "2000-01-01"      # start of back test
to            = "2003-12-31"

tradesize     <- 1000            # size risked on one trade
initeq        <- 100000          # total account equity

Sys.setenv(TZ = "UTC")
currency("USD")

############################# GET DATA ######################################

getSymbols(sym, 
           from = from, 
           to = to,
           index.class=c("POSIXt","POSIXct"))

############################# PLOT DATA ######################################

plot(Cl(LQD))

# plot with indicators
chartSeries(LQD,
            theme = chartTheme('white'),
            TA = "addBBands(); addDEMA()")
addVo()
addDPO()

############################# INITIALIZE ####################################
# remove strategy if it already exists
rm.strat(st.1)
rm.strat(port)
rm.strat(acct)

sym <- stock(sym ,
      currency='USD', 
      multiplier=1)

initPortf(port, 
          symbols = sym, 
          initDate=initdate)

initAcct(acct, 
         portfolios = port, 
         initEq = initeq, 
         initDate = initdate)

initOrders(port, 
           initDate=initdate)

# Store the strategy
strategy(st.1, store = TRUE)

############################# MAX POSITION LOGIC ############################

addPosLimit(
  portfolio=port,
  symbol=sym, 
  timestamp=initdate,  
  maxpos=100)


############################# INDICATORS ####################################

add.indicator(strategy = st.1, 
              name = "SMA", 
              arguments = list(x = quote(Cl(mktdata)), n = 50), 
              label = "SMA50")

add.indicator(strategy = st.1,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)), n = 200),
              label = "SMA200")


test <- applyIndicators(strategy = st.1,
                        mktdata = OHLC(LQD))

head(test, 3)
tail(test, n = 3)
test["1999-09-01/2010-09-05"]



############################# SIGNALS #######################################

# trading when 50 SMA < 200 SMA
add.signal(st.1,
           name = "sigComparison",
           arguments = list(columns = c("SMA50", "SMA200"),
                            relationship = "lt"),
           label = "down")


# crossover 50SMA > 200 SMA signal
add.signal(st.1,
           name = "sigCrossover",
           arguments = list(columns = c("SMA50", "SMA200"),
                            relationship = "gt"),
           label = "up")


# add signals to strategy
test_init <- applyIndicators(st.1, mktdata = OHLC(LQD))
test <- applySignals(strategy = st.1, mktdata = test_init)
tail(test, 3)

############################# RULES #########################################
# long
# enter trade
add.rule(strategy = st.1, 
         name = "ruleSignal", 
         arguments=list(sigcol = "up", 
                        sigval = TRUE, 
                        orderqty = 1,
                        ordertype = "market",
                        orderside = "long",
                        replace = FALSE, 
                        prefer = "Open"),
         type = "enter",
         label = "EnterLong")

# exit trade
add.rule(strategy = st.1,
         name = "ruleSignal", 
         arguments = list(sigcol = "down", 
                          sigval = TRUE, 
                          orderqty = "all", 
                          ordertype = "market", 
                          orderside = "long", 
                          replace = FALSE, 
                          prefer = "Open"), 
         type = "exit",
         label = "ExitLong")

# short
# enter trade
add.rule(strategy = st.1, 
         name = "ruleSignal", 
         arguments=list(sigcol = "down", 
                        sigval = TRUE, 
                        orderqty = 1,
                        ordertype = "market",
                        orderside = "short",
                        replace = FALSE, 
                        prefer = "Open"),
         type = "enter",
         label = "EnterShort")

# exit trade
add.rule(strategy = st.1,
         name = "ruleSignal", 
         arguments = list(sigcol = "up", 
                          sigval = TRUE, 
                          orderqty = "all", 
                          ordertype = "market", 
                          orderside = "short", 
                          replace = FALSE, 
                          prefer = "Open"), 
         type = "exit",
         label = "ExitShort")


############################# APPLY STRATEGY ################################

# apply strategy
applyStrategy(st.1, 
              port, 
              prefer='Open', 
              verbose=FALSE)

############################# UPDATE ########################################

# update portfolio
updatePortf(port)
daterange <- time(getPortfolio(port)$summary)[-1]

# update account
updateAcct(acct, daterange)
updateEndEq(acct)



########################### STATISTICS #############################

invisible(mktdata)
tStats   <- tradeStats(port)
rets <- PortfReturns(acct)
getOrderBook(port)
getTxns(port, sym)

cat('Profit Factor: ', tStats$Profit.Factor, '\n')
cat('Percent Positive: ', tStats$Percent.Positive, '\n')
cat('Max drawdown: ', maxDrawdown(rets), '\n')

# Sharpe ratio - reward to risk
# geometic = FALSE means profits are withdrawn form the system as they occur
cat('Sharpe Ratio: ', SharpeRatio.annualized(rets, geometric = FALSE), '\n')

############################# VISUALIZE #################################
# cumulative increase
# drawdown - $ lost from the last high

chart.Posn(Portfolio = port, Symbol = "LQD")

# add indicators to chart
sma50 <- SMA(Cl(LQD), 50)
sma200 <- SMA(Cl(LQD), 200)

chart.Posn(Portfolio = port, Symbol = "LQD")
add_TA(sma50, on = 1, col = "blue")
add_TA(sma200, on = 1, col = "red")

# zoon on chart
zoomChart('2001-08::')


################################################################

