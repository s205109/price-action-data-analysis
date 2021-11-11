setwd("/Users/dominicsciarrino/Documents/projects/trading/price-action-data-analysis")

library(dplyr)
library(ggplot2)
library(plotly)

# Author: Dominic Sciarrino
# Date: 11/11/2021

#
# Define the path to the processed data folder
#
data_processed_path <- paste(getwd(), "/data/processed", sep="")

# Processed data for DOW 30 5Y 1d data
DOW30_5Y1d_data_proc_path <- paste(data_processed_path, "/5Y-1d_DOW30", sep="")

# Processed data for NASDAQ 100 5Y 1d data
NASDAQ_5Y1d_data_proc_path <- paste(data_processed_path, "/5Y-1d_NASDAQ100", sep="")

# List the files for the DOW 30
DOW30_5Y1d_files <- list.files(
  path = DOW30_5Y1d_data_proc_path
)

# List the files for the NASDAQ 100
NASDAQ100_5Y1d_files <- list.files(
  path = NASDAQ_5Y1d_data_proc_path
)

# Create summarizing frame
gap_strategy_summary <- data.frame(Ticker=character(),
                                   StrategyReturn=double(),
                                   BaseReturn=double())

for (k in seq(from=1, to=length(DOW30_5Y1d_files))){
  df_pricing <- read.csv(paste(DOW30_5Y1d_data_proc_path, DOW30_5Y1d_files[k], sep="/"))
  
  ####################################################################
  #               BACKTEST DOWNWARD GAP DETECTION                    #
  ####################################################################
  buy_orders <- c()
  sell_orders <- c()
  returns <- c()
  base_returns <- c()
  
  trade_currently_executed <- FALSE
  current_trade_buy_price <- 0
  current_trade_sell_target <- 0
  current_trade_stop_target <- 0
  STOP_LOSS_PERCENT <- 1
  base_principal <- 1
  strategy_principal <- 1
  current_ticker <- df_pricing[k,]$Ticker[1]
  
  for (i in seq(from=1, to=nrow(df_pricing))) {
    return <- 0
    
    # Gap Down
    if (i != 1 && df_pricing[i-1,]$Low > df_pricing[i,]$Open && trade_currently_executed == FALSE){
      # Create a buy order for the open of day i
      buy_orders <- c(buy_orders, df_pricing[i,]$Open)
      current_trade_buy_price <- df_pricing[i,]$Open
      
      # Create a sell target for the future
      current_trade_sell_target <- df_pricing[i-1,]$Low
      current_trade_stop_target <- current_trade_buy_price * (1-STOP_LOSS_PERCENT)
      
      # Program flag to make sure only one trade is open at a time.
      trade_currently_executed <- TRUE
    }
    # Not a Gap Down
    else {
      # Zero out the buy order
      buy_orders <- c(buy_orders, 0)
      
      # Trade currently happening? No -> Zero it out
      if(trade_currently_executed == FALSE){
        current_trade_sell_target <- 0
        current_trade_stop_target <- 0
      }
    }
    
    # Meeting the Stop Target
    if(trade_currently_executed && df_pricing[i,]$Low < current_trade_stop_target){
      sell_orders <- c(sell_orders, current_trade_stop_target)
      return <- 1+((current_trade_stop_target-current_trade_buy_price)/current_trade_buy_price)
      returns <- c(returns, return)
      trade_currently_executed <- FALSE
      current_trade_buy_price <- 0
      current_trade_sell_target <- 0
      current_trade_stop_target <- 0
    }
    
    # Meeting the Sell Target
    else if(trade_currently_executed && df_pricing[i,]$High > current_trade_sell_target){
      sell_orders <- c(sell_orders, current_trade_sell_target)
      return <- 1+((current_trade_sell_target-current_trade_buy_price)/current_trade_buy_price)
      returns <- c(returns, return)
      trade_currently_executed <- FALSE
      current_trade_buy_price <- 0
      current_trade_sell_target <- 0
      current_trade_stop_target <- 0
    }
    
    # Neither of the sell signals are met
    else{
      sell_orders <- c(sell_orders, 0)
      returns <- c(returns, 1)
    }
    
    # Calculate Cumulative Base Returns
    if (i == 1){
      base_return = 1
    }else{
      base_return <- ((df_pricing[i,]$Close - df_pricing[i,]$Open) / df_pricing[i,]$Open)
    }
    base_returns <- c(base_returns, base_return)
    base_principal <- base_principal * (1+base_return)
    
    # Calculate Strategy Returns
    strategy_principal <- strategy_principal * returns[i]
  }
  
  ###########################################
  #       UPDATE BACKTEST DATA FRAME        #
  ###########################################
  df_pricing$Buy_Orders <- buy_orders
  df_pricing$Sell_Orders <- sell_orders
  df_pricing$StrategyReturns <- returns
  df_pricing$BaseReturns <- base_returns
  
  gap_strategy_summary[nrow(gap_strategy_summary)+1,] = c(current_ticker,
                                                          strategy_principal,
                                                          base_principal)
}


for (k in seq(from=1, to=length(NASDAQ100_5Y1d_files))){
  df_pricing <- read.csv(paste(NASDAQ_5Y1d_data_proc_path, NASDAQ100_5Y1d_files[k], sep="/"))
  
  ####################################################################
  #               BACKTEST DOWNWARD GAP DETECTION                    #
  ####################################################################
  buy_orders <- c()
  sell_orders <- c()
  returns <- c()
  base_returns <- c()
  
  trade_currently_executed <- FALSE
  current_trade_buy_price <- 0
  current_trade_sell_target <- 0
  current_trade_stop_target <- 0
  STOP_LOSS_PERCENT <- 1
  base_principal <- 1
  strategy_principal <- 1
  current_ticker <- df_pricing[k,]$Ticker[1]
  
  for (i in seq(from=1, to=nrow(df_pricing))) {
    return <- 0
    
    # Gap Down
    if (i != 1 && df_pricing[i-1,]$Low > df_pricing[i,]$Open && trade_currently_executed == FALSE){
      # Create a buy order for the open of day i
      buy_orders <- c(buy_orders, df_pricing[i,]$Open)
      current_trade_buy_price <- df_pricing[i,]$Open
      
      # Create a sell target for the future
      current_trade_sell_target <- df_pricing[i-1,]$Low
      current_trade_stop_target <- current_trade_buy_price * (1-STOP_LOSS_PERCENT)
      
      # Program flag to make sure only one trade is open at a time.
      trade_currently_executed <- TRUE
    }
    # Not a Gap Down
    else {
      # Zero out the buy order
      buy_orders <- c(buy_orders, 0)
      
      # Trade currently happening? No -> Zero it out
      if(trade_currently_executed == FALSE){
        current_trade_sell_target <- 0
        current_trade_stop_target <- 0
      }
    }
    
    # Meeting the Stop Target
    if(trade_currently_executed && df_pricing[i,]$Low < current_trade_stop_target){
      sell_orders <- c(sell_orders, current_trade_stop_target)
      return <- 1+((current_trade_stop_target-current_trade_buy_price)/current_trade_buy_price)
      returns <- c(returns, return)
      trade_currently_executed <- FALSE
      current_trade_buy_price <- 0
      current_trade_sell_target <- 0
      current_trade_stop_target <- 0
    }
    
    # Meeting the Sell Target
    else if(trade_currently_executed && df_pricing[i,]$High > current_trade_sell_target){
      sell_orders <- c(sell_orders, current_trade_sell_target)
      return <- 1+((current_trade_sell_target-current_trade_buy_price)/current_trade_buy_price)
      returns <- c(returns, return)
      trade_currently_executed <- FALSE
      current_trade_buy_price <- 0
      current_trade_sell_target <- 0
      current_trade_stop_target <- 0
    }
    
    # Neither of the sell signals are met
    else{
      sell_orders <- c(sell_orders, 0)
      returns <- c(returns, 1)
    }
    
    # Calculate Cumulative Base Returns
    if (i == 1){
      base_return = 1
    }else{
      base_return <- ((df_pricing[i,]$Close - df_pricing[i,]$Open) / df_pricing[i,]$Open)
    }
    base_returns <- c(base_returns, base_return)
    base_principal <- base_principal * (1+base_return)
    
    # Calculate Strategy Returns
    strategy_principal <- strategy_principal * returns[i]
  }
  
  ###########################################
  #       UPDATE BACKTEST DATA FRAME        #
  ###########################################
  df_pricing$Buy_Orders <- buy_orders
  df_pricing$Sell_Orders <- sell_orders
  df_pricing$StrategyReturns <- returns
  df_pricing$BaseReturns <- base_returns
  
  gap_strategy_summary[nrow(gap_strategy_summary)+1,] = c(current_ticker,
                                                          strategy_principal,
                                                          base_principal)
}





count = 0
for(i in seq(from=1, to=nrow(gap_strategy_summary))){
  if(gap_strategy_summary[i,]$StrategyReturn > gap_strategy_summary[i,]$BaseReturn){
    count = count + 1
  }
}
count / nrow(gap_strategy_summary)
mean(as.double(gap_strategy_summary$StrategyReturn))
mean(as.double(gap_strategy_summary$BaseReturn))
View(gap_strategy_summary)
