
#From Financial analytics with R read Chapter 3.7, 3.8, 7.1

#Download data for five stocks of your choice + SPY
#####Problem 1#####
# Read Chapters 3.7 and 3.8 and calculate the alpha and beta for each
# of the five stocks
#####Problem 1#####
library(tidyverse)
library(tidyquant)
library(lubridate)
library(RcppRoll)

# AMZN, KO, NVDA, AAPL, TSLA

spy <- getSymbols("SPY", src = "yahoo", from = "2011-01-01") %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  'colnames<-'("SPY")

market_return <- na.omit(Return.calculate(spy, method = "log"))

amzn <- getSymbols("AMZN", src = "yahoo", from = "2011-01-01") %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  'colnames<-'("AMZN")
amzn_return <- na.omit(Return.calculate(amzn, method = "log"))
amzn_alpha = CAPM.alpha(amzn, market_return)
amzn_beta = CAPM.beta(amzn, market_return)

ko <- getSymbols("KO", src = "yahoo", from = "2011-01-01") %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  'colnames<-'("KO")
ko_return <- na.omit(Return.calculate(ko, method = "log"))
ko_alpha = CAPM.alpha(ko, market_return)
ko_beta = CAPM.beta(ko, market_return)

nvda <- getSymbols("NVDA", src = "yahoo", from = "2011-01-01") %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  'colnames<-'("NVDA")
nvda_return <- na.omit(Return.calculate(nvda, method = "log"))
nvda_alpha = CAPM.alpha(nvda, market_return)
nvda_beta = CAPM.beta(nvda, market_return)

aapl <- getSymbols("AAPL", src = "yahoo", from = "2011-01-01") %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  'colnames<-'("AAPL")
aapl_return <- na.omit(Return.calculate(aapl, method = "log"))
aapl_alpha = CAPM.alpha(aapl, market_return)
aapl_beta = CAPM.beta(aapl, market_return)

tsla <- getSymbols("TSLA", src = "yahoo", from = "2011-01-01") %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  'colnames<-'("TSLA")
tsla_return <- na.omit(Return.calculate(tsla, method = "log"))
tsla_alpha = CAPM.alpha(tsla, market_return)
tsla_beta = CAPM.beta(tsla, market_return)


#####Problem 2#####
# Simulate the efficient frontier of a portfolio created by the 5 stocks
# you have chosen. Here is a solution written in Python.
# https://www.interviewqs.com/blog/efficient-frontier
# Pay attention that you should use portfolio returns: The percentage
# change in the stock price: price-lag(price)/lag(price)

# Create a table with randomized weights, which add up to one.
# I'd suggest to start with 5 columns - one for each of 
# the stocks, which contain the corresponding weights.
# Then you can add twenty-three additional columns:
# Five columns with the expected return for each of the stocks.
# Five columns with the standard deviation for each of the stocks.
# Ten columns with the covariances between each two stocks(5*4/2 = 10)
# Two column for Expected return and the standard deviation of the portfolio
# One column for Sharpe ratio, which you will calculate and add later.
# This is the formula for the sum of expected values - E[aX+bY] = aE[X]+ bE[Y].
# The formula for variances is Var(aX+aY)=a^2*Var(X)+b^2*Var(Y)+2Cov(X,Y). 
# And then take the square root to get the standard deviation.
# Calculate the Sharpe ratio, using risk free rate of 1% - look at chapter 7.1.
# Choose the portfolio with the highest Sharpe ratio.
#####Problem 2#####