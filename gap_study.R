setwd("/Users/dominicsciarrino/Documents/projects/trading/price-action-data-analysis")

library(dplyr)
library(ggplot2)
library(plotly)

# Author: Dominic Sciarrino
# Date: 11/18/2021

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

all_files <- c()
for (f in DOW30_5Y1d_files){
  all_files <- c(all_files, paste(DOW30_5Y1d_data_proc_path, f, sep="/"))
}
for (f in NASDAQ100_5Y1d_files){
  all_files <- c(all_files, paste(NASDAQ_5Y1d_data_proc_path, f, sep="/"))
}

# Create summarizing frame
gap_strategy_summary <- data.frame(Ticker=character(),
                                   TotalReturn=double(),
                                   Drawdown=double(),
                                   StandardDev=double())


STOP_LOSS_PERCENT <- 0.10

for (k in seq(from=1, to=length(all_files))){
  # Get CSV pricing data
  df_pricing <- read.csv(all_files[k])
  current_ticker <- df_pricing[k,]$Ticker[1]

  ####################################################################
  #               BACKTEST DOWNWARD GAP DETECTION                    #
  ####################################################################
  buy_orders <- c()
  sell_orders <- c()
  returns <- c()
  investment <- c()
  trade_execution_status <- c()
  exposed_returns <- c()

  trade_currently_executed <- FALSE
  current_trade_buy_price <- 0
  current_trade_sell_target <- 0
  current_trade_stop_target <- 0
  strategy_total <- 1

  for (i in seq(from=1, to=nrow(df_pricing))) {

    #################################
    #      Detect Buy Signals       #
    #################################

    # Detect Downward Gap
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
    # Downward Gap NOT detected
    else {
      # Zero out the buy order
      buy_orders <- c(buy_orders, 0)
      
      # Trade currently happening? No -> Zero it out
      if(trade_currently_executed == FALSE){
        current_trade_sell_target <- 0
        current_trade_stop_target <- 0
      }
    }

    #################################
    #      Detect Sell Signals      #
    #################################

    # Meeting the Stop Target
    if(trade_currently_executed && df_pricing[i,]$Low <= current_trade_stop_target){
      sell_orders <- c(sell_orders, current_trade_stop_target)
      r <- 1+((current_trade_stop_target-current_trade_buy_price)/current_trade_buy_price)
      trade_currently_executed <- FALSE
      current_trade_buy_price <- 0
      current_trade_sell_target <- 0
      current_trade_stop_target <- 0
    }

    # Meeting the Sell Target
    else if(trade_currently_executed && df_pricing[i,]$High > current_trade_sell_target){
      sell_orders <- c(sell_orders, current_trade_sell_target)
      r <- 1+((current_trade_sell_target-current_trade_buy_price)/current_trade_buy_price)
      trade_currently_executed <- FALSE
      current_trade_buy_price <- 0
      current_trade_sell_target <- 0
      current_trade_stop_target <- 0
    }
    
    # Neither of the sell signals are met
    else{
      sell_orders <- c(sell_orders, 0)
      r<-1
    }

    #################################
    #         Update Returns        #
    #################################

    # Calculate Strategy Returns
    returns <- c(returns, r)
    strategy_total <- strategy_total * r
    investment <- c(investment, strategy_total)
    
    # Calculate exposed returns
    exp_ret <- 1+((df_pricing[i,]$Close-df_pricing[i,]$Open)/df_pricing[i,]$Open)
    if (trade_currently_executed){
      exposed_returns <- c(exposed_returns, exp_ret)
    }
    else {
      exposed_returns <- c(exposed_returns, 1)
    }

    # Track when we're in a trade
    trade_execution_status <- c(trade_execution_status, trade_currently_executed)
    
  }

  ##########################################
  #         Update Strategy Summary        #
  ##########################################
  
  # Calculate drawdown
  max_dd <- mdd(exposed_returns, ex_stat=trade_execution_status)
  
  # Calculate standard deviation
  stddev <- sd(exposed_returns)
  
  gap_strategy_summary[nrow(gap_strategy_summary)+1,] = c(current_ticker,
                                                          strategy_total,
                                                          max_dd,
                                                          stddev)

}


gap_strategy_summary <- gap_strategy_summary %>% distinct() %>% arrange(Ticker)
View(gap_strategy_summary)

barplot(as.double(gap_strategy_summary$TotalReturn)-1, main=expression(paste("Gap Down Strategy Returns (", beta, "=10%)")), names.arg=gap_strategy_summary$Ticker, las=2, cex.names=0.5, col=ifelse(gap_strategy_summary$TotalReturn<1,"red","lightgreen"))
mean(as.double(gap_strategy_summary$TotalReturn))

barplot(as.double(gap_strategy_summary$StandardDev), main="Gap Down Strategy Standard Deviation", names.arg=gap_strategy_summary$Ticker, las=2, cex.names=0.5, col="lightblue")
mean(as.double(gap_strategy_summary$StandardDev))

barplot(as.double(gap_strategy_summary$Drawdown), main="Gap Down Strategy Max Drawdowns", names.arg=gap_strategy_summary$Ticker, las=2, cex.names=0.5, col="orange")
mean(as.double(gap_strategy_summary$Drawdown))







