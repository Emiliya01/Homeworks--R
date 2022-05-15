library(tidyverse)
library(tidyquant)
library(lubridate)

#####Problem 1#####
# 1.Download the stock prices for AMZN, FB, NFLX, stocks from 2019-01-01 
# to 2021-04-01. Keep only the symbol/date/adjusted columns.
# 2.Add all the missing dates(such as 2019-01-01), so that we have 
# observations for every single date. Fill in the missing values for adjusted 
# with the last non-missing observation.
# 3.Create a new data frame, which consist only of stocks from AMZN or FB and 
# has observations from 2019-01-01 to 2019-07-01 or 2020-04-01 to 2020-07-01. 
# Arrange the data frame first by the symbol name and by the date in 
# descending order.
# 4.Select the first and last observation of the aforementioned dataframe
# for each of the two stocks - AMZN and FB.
# 5.Select the last observation for each stock, for each month. 
# In order to do this, first create a new column, which will show you the 
# year and the month. You can do this using the functions substr() or floor_date.

#1
data <- tq_get(c("AMZN", "FB", "NFLX"), get = "stock.prices",
               from = "2019-01-01", to = "2021-04-01") %>%
  select(symbol, date, adjusted)

data

#2
Dates <- data.frame(Dates = seq.Date(from = ymd("2019-01-01"),
                                     to = ymd("2021-04-01"),
                                     by = "day"),
                    Symbol = c(rep("AMZN", 822),rep("FB", 822), rep("NFLX", 822)))

Dates


Join <- Dates %>%
  dplyr::left_join(data, by = c("Dates" = "date" , "Symbol" = "symbol"))%>%
  group_by(Symbol)%>%
  fill(adjusted, .direction = "downup")

View(Join)


#3

Stock_FB <- data.frame(Date = seq.Date(from = ymd("2020-04-01"),
                                       to = ymd("2020-07-01"),
                                       by = "day"),
                       Symbol = c(rep("FB", 92)))



Stock_AMZN <- data.frame(Date = seq.Date(from = ymd("2019-01-01"),
                                         to = ymd("2019-07-01"),
                                         by = "day"),
                         Symbol = c(rep("AMZN", 182)))



Data_frame <- full_join(Stock_AMZN, Stock_FB, by=c("Date", "Symbol")) 

Final <- Data_frame %>%
  dplyr::left_join(data, by = c("Date" = "date" , "Symbol" = "symbol"))%>%
  fill(adjusted, .direction = "downup") %>%
  arrange(desc(Date)) %>%
  arrange(Symbol) 

View(Final)

#4

Final %>%
  group_by(Symbol) %>%
  slice_head()

Final %>%
  group_by(Symbol) %>%
  slice_tail()


#5

Modif_Final <- Final %>%
  mutate(Date_month = floor_date(Date, "month"))

Modif_Final

Modif_Final%>%
  group_by(Date_month)%>%
  slice_head()


#####Problem 2#####
#Use the dataframe from problem 1.2.
# Use the SMA function from the tidyquant package to calculate the 10day SMA 
# and the 26 day SMA for each of the 3 stocks. 
# How many times did the 10 day SMA line cross 26 day SMA line from below? 
# How many times did the 10 day SMA line cross 26 day SMA line from above?
# You can take a look at this article: https://www.investopedia.com/trading/macd/
# Essentially by cross from above/below I want you to find the buy/sell signals.

SMA <- Join%>%
  mutate(SMA10 = SMA(adjusted, n = 10), SMA26 = SMA(adjusted, n = 26))

View(SMA)

SMA[["SMA10"]][100]

above = 0
below = 0 
for(i in 1:(nrow(SMA)-1)){
  if(!is.na(SMA[["SMA10"]][i])&&!is.na(SMA[["SMA10"]][i+1])&&!is.na(SMA[["SMA26"]][i])&&!is.na(SMA[["SMA26"]][i+1])){
    
    if(SMA[["SMA10"]][i]-SMA[["SMA26"]][i]>0 && SMA[["SMA10"]][i+1]-SMA[["SMA26"]][i+1]<0){
      above = above + 1
      print("above")
      print(above)
    } else if (SMA[["SMA10"]][i]-SMA[["SMA26"]][i]<0 && SMA[["SMA10"]][i+1]-SMA[["SMA26"]][i+1]>0){
      below = below + 1
      print("below")
      print(below)
    }
  }
}

above
below
