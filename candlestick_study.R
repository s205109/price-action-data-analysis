setwd("/Users/dominicsciarrino/Documents/projects/trading/price-action-data-analysis")

library(dplyr)
library(ggplot2)
library(plotly)

# Author: Dominic Sciarrino
# Date: 11/11/2021

# Constants
HOLD_RANGE <- 3

# ERROR & THRESHOLD PARAMETERS
MARUBOZU_ERROR <- 0.07          # 7%
MARUBOZU_THRESHOLD <- 0.80      # 80%

# MARUBOZU CANDLESTICK TYPES
MARUBOZU_BULL_OPENING   = "BULL OPENING MARUBOZU"
MARUBOZU_BULL_CLOSING   = "BULL CLOSING MARUBOZU"
MARUBOZU_BULL_IDEAL     = "IDEAL BULL MARUBOZU"
MARUBOZU_BEAR_OPENING   = "BEAR OPENING MARUBOZU"
MARUBOZU_BEAR_CLOSING   = "BEAR CLOSING MARUBOZU"
MARUBOZU_BEAR_IDEAL     = "IDEAL BEAR MARUBOZU"

#
# Define the path to the processed data folder
#
data_processed_path <- paste(getwd(), "/data/processed", sep="")

# Processed data for DOW 30 5Y 1d data
DOW30_5Y1d_data_proc_path <- paste(data_processed_path, "/5Y-1d_DOW30", sep="")
# List the files for the DOW 30 files
DOW30_5Y1d_files <- list.files(
  path = DOW30_5Y1d_data_proc_path
)



# Data Frame for Backtest results
backtest_df <- data.frame(Ticker=character(), Returns=double(), Base_Return=double())


for (k in seq(from=1, to=length(DOW30_5Y1d_files))){
  #########################################
  #         GET STOCK TO ANALYZE          #
  #########################################
  df_pricing <- read.csv(paste(DOW30_5Y1d_data_proc_path, DOW30_5Y1d_files[k], sep="/"))
  df_pricing$Date <- as.Date(df_pricing$Date, format="%m-%d-%y")   ## Format to Date class 
  current_ticker <- df_pricing$Ticker[k]  ## Get the ticker of current stock

  base_return <- 1 + ((df_pricing[nrow(df_pricing),]$Close - df_pricing[1,]$Open) / df_pricing[1,]$Open)
  
  # Calculate daily returns
  returns <- c(0)
  for(i in seq(from=1, to=nrow(df_pricing))){
    return <- (df_pricing[i,]$Close - df_pricing[i-1,]$Close) / df_pricing[i-1,]$Close
    returns <- c(returns, return)
  }
  df_pricing <- df_pricing %>% mutate(Return = returns)
  
  ############################################
  #          DETECT MARUBOZU CANDLES         #
  ############################################
  
  candlesticks = c()
  
  for(i in seq(from=1, to=nrow(df_pricing))){
    direction = "NONE"
    r = 0
    b = 0
    b_avg = 0
    m = 0
    if (is.nan(df_pricing[i,]$Open) || is.nan(df_pricing[i,]$Close)){
      direction = NaN
      r = 0
      b = 0
      b_avg = 0
      m = 0
    }
    
    # Set Direction
    if (df_pricing[i,]$Open < df_pricing[i,]$Close) {
      direction = "UP"
      b = df_pricing[i,]$Close - df_pricing[i,]$Open
      b_avg = (b/2) + df_pricing[i,]$Open
    }
    if (df_pricing[i,]$Open > df_pricing[i,]$Close) {
      direction = "DOWN"
      b = df_pricing[i,]$Open - df_pricing[i,]$Close
      b_avg = (b/2) + df_pricing[i,]$Close
    }
    if (df_pricing[i,]$Open == df_pricing[i,]$Close) {
      direction = "NONE"
      b = 0
      b_avg = df_pricing[i,]$Close
    }
  
    # Set price range
    r = df_pricing[i,]$High - df_pricing[i,]$Low
  
    # Set midpoint/mid-price
    m = (df_pricing[i,]$High + df_pricing[i,]$Low) / 2
    
    # Marubozu Error = u_m * candlestick_range
    marubozuError <- MARUBOZU_ERROR * r
    candlestick <- ""
    
    # BULL OPENING MARUBOZU
    #
    # Marubozu candles are defined by the body taking up 80% or more of the 
    # candle's price range.
    #
    # A Bull Opening Marubozu is one where the opening price is very close to 
    # the Low of the day.
    if( (b > (r * MARUBOZU_THRESHOLD)) &&
        (direction == "UP") &&
        ((df_pricing[i,]$Low + marubozuError) > df_pricing[i,]$Open) ){
      candlestick <- MARUBOZU_BULL_OPENING
    }
    
    # DETECT BULL CLOSING MARUBOZU
    #
    # A Bull Closing Marubozu is one where the closing price is very close to the
    # High of the day.
    if( (b > (r * MARUBOZU_THRESHOLD)) &&
        (direction == "UP") &&
        ((df_pricing[i,]$High - marubozuError) < df_pricing[i,]$Close) ){
      candlestick <- MARUBOZU_BULL_CLOSING
    }
    
    # IDEAL BULL MARUBOZU
    #
    # An ideal Bull Marubozu is one where the closing price is very close to the 
    # high and the opening price is very close to the low.
    if( (b > (r * MARUBOZU_THRESHOLD)) &&
        (direction == "UP") &&
        ((df_pricing[i,]$High - marubozuError) < df_pricing[i,]$Close) &&
        ((df_pricing[i,]$Low + marubozuError) > df_pricing[i,]$Open) ){
      candlestick <- MARUBOZU_BULL_IDEAL
    }
    
    # Add candlestick to column candelsticks
    candlesticks = c(candlesticks, candlestick)
  }
  
  df_pricing <- df_pricing %>% mutate(Candlestick = candlesticks)
  
  ##############################################
  #           Perform BackTesting              #
  ##############################################
  skip = 0
  exit_dates = c()
  stop_losses = c()
  entries = c()
  trade_outcomes = c()
  sell_dates <- c()
  sell_prices <- c()
  for (i in seq(from=1, to=nrow(df_pricing))){
    candlestick = df_pricing[i,]$Candlestick
    
    # If we previously detected a marubozu during the execution of a trade, skip
    if (candlestick != "" && skip > 0){
      skip = skip - 1
      
      sell_dates <- c(sell_dates, 0)
      trade_outcomes <- c(trade_outcomes, 1)
      exit_dates <- c(exit_dates, 0)
      stop_losses <- c(stop_losses, -1)
      sell_prices <- c(sell_prices, -1)
      entries <- c(entries, -1)
    }
    # Marubozu Candle detected? Yes->Record the date of exit.
    else if(skip == 0 && candlestick != "" && (i + HOLD_RANGE < nrow(df_pricing))){
      exit_date <- df_pricing[i + HOLD_RANGE,]$Date
      stop_loss <- df_pricing[i,]$Open
      sell_price <- stop_loss
      
      ret <- 1
      for(j in seq(from=i+1, to=(i+HOLD_RANGE))){
        if(df_pricing[j,]$Candlestick != ""){
          skip = skip + 1
        }
        # Check to see if we've hit the stop loss
        if (df_pricing[j,]$Low <= stop_loss){
          ret <- ((stop_loss - df_pricing[i,]$Close) / df_pricing[i,]$Close) + 1
          sell_price <- stop_loss
          break
        }
        ret <- ret * (1 + df_pricing[j,]$Return)
        sell_price <- df_pricing[j,]$Close
      }
      sell_date <- as.Date(df_pricing[j,]$Date, format="%m-%d-%y")
      
      trade_outcomes <- c(trade_outcomes, ret)
      exit_dates <- c(exit_dates, exit_date)
      stop_losses <- c(stop_losses, df_pricing[i,]$Open)
      entries <- c(entries, df_pricing[i,]$Close)
      sell_dates <- c(sell_dates, sell_date)
      sell_prices <- c(sell_prices, sell_price)
    }
    else {
      sell_dates <- c(sell_dates, 0)
      trade_outcomes <- c(trade_outcomes, 1)
      exit_dates <- c(exit_dates, 0)
      stop_losses <- c(stop_losses, -1)
      sell_prices <- c(sell_prices, -1)
      entries <- c(entries, -1)
    }
  }
  
  # Convert Date types
  exit_dates <- as.Date(exit_dates, origin="1970-01-01")
  sell_dates <- as.Date(sell_dates, origin="1970-01-01")
  
  # Add the constructed columns to main dataframe
  df_pricing <- df_pricing %>% mutate(
                                Entry=entries,
                                Exit_Date = exit_dates,
                                Sell_Price = sell_prices,
                                Sell_Date = sell_dates,
                                Stop_Loss=stop_losses,
                                Outcomes=trade_outcomes)
  
  
  
  # Select all the rows where a marubozu trade occurred
  trade_returns <- df_pricing %>% filter(Candlestick != "") %>% select(Ticker, Date, Entry, Sell_Date, Outcomes)
  
  # Calculate returns for those trades
  tot_ret <- 1
  for (i in seq(from=1, to=nrow(trade_returns))){
    tot_ret <- tot_ret * trade_returns[i,]$Outcomes
  }
  
  base_return
  backtest_df[nrow(backtest_df)+1,] <- c(current_ticker, tot_ret, base_return)
}


mean(as.double(backtest_df$Returns))
View(backtest_df)
