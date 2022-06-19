library(tidyverse)
library(tidyquant)
library(lubridate)

#####Problem 1##### 

#1.1
SMA_function <- function(vector, period){
  results <- NULL 
  if(period > length(vector)){
    stop("Period is out of range")
  }else{
    for (i in 1:(length(vector)-period+1)){ 
      results[i+period-1] <- c(mean(data[i:(i+period-1)])) 
    } 
  }
  return(results) 
}

data <- c(2, 4, 7, 5, 3, 0, 14, 17, 0, 15, 2)
SMA_function(data, 4)
SMA(data, 4)

#1.2
vector_1 <- c(600,800,1000) 
vector_2 <- c(1200, 1000,2000)

COR_function <- function(vector_1, vector_2){
  num <- sum((vector_1 - mean(vector_1)) * (vector_2 - mean(vector_2)))
  denom <- sqrt(sum((vector_1 - mean(vector_1))^2) * sum((vector_2- mean(vector_2))^2))
  r <- num/denom
  
  return(r)
}

COR_function(vector_1, vector_2)
cor(vector_1, vector_2)


#####Problem 2#####

prime <- NULL
for (i in 2:100){
  isPrime = TRUE
  for (div in 2:sqrt(100)){
    if (i%%div == 0  && i > div){
      isPrime = FALSE
      break
    } 
  }
  if (isPrime){
    print(i)
  }
}




#####Problem 3#####

#3.1
data <- tq_get(c("AMZN"), get = "stock.prices",
               from = "2020-01-01", to = "2022-06-19") %>%
  select(symbol, date, adjusted)

Stock_AMZN <- data.frame(data)

EMA26 = EMA(Stock_AMZN['adjusted'], n = 26)

#3.2
EMA12 = EMA(Stock_AMZN['adjusted'], n = 12)

#3.3
MACD_line = EMA12 - EMA26

#3.4
signal_line = EMA(MACD_line, n = 9)
signal_line


#3.5
above = 0
below = 0 
for(i in 2:(length(MACD_line))){
  if(!is.na(MACD_line[i-1]) && !is.na(MACD_line[i]) && !is.na(signal_line[i-1])&&!is.na(signal_line[i])){
    
    if(MACD_line[i-1]-signal_line[i-1]>0 && MACD_line[i]-signal_line[i]<0){
      below = below + 1
      print("below")
      print(i)
    } else if (MACD_line[i-1]-signal_line[i-1]<0 && MACD_line[i]-signal_line[i]>0){
      above = above + 1
      print("above")
      print(i)
    }
  }
}
sprintf("below: %s", below)
sprintf("above: %s", above)


#3.6
money = 100
own_stock = FALSE
buy_price = 0

for(i in 2:(length(MACD_line))){
  if(!is.na(MACD_line[i-1]) && !is.na(MACD_line[i]) && !is.na(signal_line[i-1])&&!is.na(signal_line[i])){
    
    if(MACD_line[i-1]-signal_line[i-1]>0 && MACD_line[i]-signal_line[i]<0 && own_stock){
      money = (money / buy_price) * Stock_AMZN[i, 'adjusted']
      print(money)
      own_stock = FALSE
      #sell
    } else if (MACD_line[i-1]-signal_line[i-1]<0 && MACD_line[i]-signal_line[i]>0 && !own_stock){
      buy_price = Stock_AMZN[i, 'adjusted']
      own_stock = TRUE 
      #buy
    }
  }
}

sprintf("Money with strategy: %s", money)
sprintf("Money without strategy: %s", (100 / Stock_AMZN[1, 'adjusted']) * Stock_AMZN[length(MACD_line), 'adjusted'])

