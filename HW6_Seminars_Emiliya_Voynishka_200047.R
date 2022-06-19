# https://dplyr.tidyverse.org/articles/dplyr.html - the basic
# https://dplyr.tidyverse.org/articles/grouping.html - group by in details
# https://dplyr.tidyverse.org/articles/base.html - comparing dplyr functions
# to the base R function.

library(tidyverse)
library(tidyquant)
library(lubridate)
library(RcppRoll)

#####Problem 1#####
# Download data for a stock of your choice and do the following:
# Calculate the 20 day SMA of the stock price and define upper and
# lower bounds around it which are equal to SMA +/-2 standard deviation
# the past observations used to calculate the SMA.
# Employ the following strategy and compare to a baseline strategy of buy and hold:
# If the price goes above the upper bound - sell.
# If the price goes below the lower bound - buy.
#####Problem 1#####

data <- tq_get(c("AMZN"), get = "stock.prices", 
               from = "2020-01-01", to = "2022-06-19") %>%
  select(adjusted) %>%
  mutate(SMA20 = SMA(adjusted, n=20),
         SD20 = roll_sd(adjusted, n=20, fill = NA))

bounds <- data %>%
  mutate(up_bound = SMA20 + SD20 * 2,
         low_bound = SMA20 - SD20 * 2,
         execute = case_when(adjusted > up_bound ~ "sell",
                             adjusted < low_bound ~ "buy"))

money = 100
own_stock = FALSE
buy_price = 0

for(i in 1:(nrow(bounds))){
  if(!is.na(bounds[i, "execute"])){
    
    if(bounds[i, "execute"] == "sell" && own_stock){
      money = (money / buy_price) * data[i, 'adjusted']
      own_stock = FALSE
      #sell
    } else if (bounds[i, "execute"] == "sell" && !own_stock){
      buy_price = data[i, 'adjusted']
      own_stock = TRUE 
      #buy
    }
  }
}

sprintf("Money with strategy: %s", money)
sprintf("Money without strategy: %s", (100 / data[1, 'adjusted']) * data[nrow(data), 'adjusted'])


#####Problem 2#####
# Calculate the RSI using the instruction about the formula from here:
# https://www.investopedia.com/terms/r/rsi.asp
# Employ the following strategy and compare to a baseline strategy of buy and hold:
# If the RSI above 65 - sell.
# If the price goes below 35 - buy.
#####Problem 2#####

data <- tq_get(c("AMZN"), get = "stock.prices",   
               from = "2020-01-01", to = "2022-06-19") %>%
  select(adjusted) %>%
  mutate(SMA20 = SMA(adjusted, n=20),
         SD20 = roll_sd(adjusted, n=20, fill = NA))

rsi_chart <- data %>%
  mutate(RSI = RSI(adjusted, n=14),
         signals = case_when(RSI > 65 ~ "sell",
                             RSI < 35 ~ "buy"))
  
money = 100
own_stock = FALSE
buy_price = 0

for(i in 1:(nrow(rsi_chart))){
  if(!is.na(rsi_chart[i, "signals"])){
    
    if(rsi_chart[i, "signals"] == "sell" && own_stock){
      money = (money / buy_price) * data[i, 'adjusted']
      own_stock = FALSE
      #sell
    } else if (rsi_chart[i, "signals"] == "sell" && !own_stock){
      buy_price = data[i, 'adjusted']
      own_stock = TRUE 
      #buy
    }
  }
}

sprintf("Money with strategy: %s", money)
sprintf("Money without strategy: %s", (100 / data[1, 'adjusted']) * data[nrow(data), 'adjusted'])  
  
  
  
  
  
  