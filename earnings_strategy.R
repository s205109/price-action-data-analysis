setwd("/Users/dominicsciarrino/Documents/projects/trading/price-action-data-analysis")

library(dplyr)
library(ggplot2)
library(plotly)

# Author: Dominic Sciarrino
# Date: 11/19/2021


# Constants
POST_EARNINGS_RANGE <- 15

#
# Define the path to the processed data folder
#
data_processed_path <- paste(getwd(), "/data/processed", sep="")
data_processed_path

# Processed data for DOW 30 earnings data
DOW30_earnings_data_proc_path <- paste(data_processed_path, "/DOW30_Earnings", sep="")
# Processed data for DOW 30 5Y 1d data
DOW30_5Y1d_data_proc_path <- paste(data_processed_path, "/5Y-1d_DOW30", sep="")
# List the files for the DOW 30 files
DOW30_5Y1d_files <- list.files(
  path = DOW30_5Y1d_data_proc_path
)
# DOW30 Earnings Data Frame
df_earnings <- read.csv(paste(DOW30_earnings_data_proc_path, "DOW30_Earnings.csv", sep="/"))



# Processed data for NASDAQ100 5Y 1d data
NASDAQ100_5Y1d_data_proc_path <- paste(data_processed_path, "/5Y-1d_NASDAQ100", sep="")
# List the files for the NASDAQ100 files
NASDAQ100_5Y1d_files <- list.files(
  path = NASDAQ100_5Y1d_data_proc_path
)
# Processed data for NASDAQ100 earnings data
NASDAQ100_earnings_data_proc_path <- paste(data_processed_path, "/NASDAQ100_Earnings", sep="")
# NASDAQ100 Earnings Data Frame
nasdaq_earnings_df <- read.csv(paste(NASDAQ100_earnings_data_proc_path, "NASDAQ100_Earnings.csv", sep="/"))

all_files <- c()
for (f in DOW30_5Y1d_files){
  all_files <- c(all_files, paste(DOW30_5Y1d_data_proc_path, f, sep="/"))
}
for (f in NASDAQ100_5Y1d_files){
  all_files <- c(all_files, paste(NASDAQ100_5Y1d_data_proc_path, f, sep="/"))
}

df_merged <- merge(df_earnings, nasdaq_earnings_df, all=TRUE)

earnings_strategy_df <- data.frame(Ticker=character(),
                                   Return=double(),
                                   MaxDrawdown=double(),
                                   StandardDev=double())
                                   

for(k in seq(from=1, to=length(all_files))){
  # Get Pricing Data
  df_pricing <- read.csv(all_files[k])
  
  # Extract the date column from the pricing data
  df_pricing$Date <- as.Date(df_pricing$Date, format="%m-%d-%y")   ## Format to Date class 
  # Extract the ticker of the current stock
  current_ticker <- df_pricing$Ticker[1]
  
  # Filter the earnings data so only the current stock is considered
  current_earnings_data <- df_merged %>% filter(Ticker == current_ticker)
  # Extract the date column from the current earnings data frame
  current_earnings_data$Date <- as.Date(current_earnings_data$Date, format="%m-%d-%y", origin="01-01-70")
  current_earnings_data <- current_earnings_data %>% arrange(Date)

  post_dates <- c()
  earnings_announced <- c()
  trade_status <- c()
  earnings_dates <- current_earnings_data$Date

  buy_back_date <- NULL
  trade_executed <- TRUE
  e <- 1
  for(i in seq(from=1, to=nrow(df_pricing))){
    # Determine the post-earnings zone dates
    if( (i + POST_EARNINGS_RANGE) >= nrow(df_pricing) ){
      post_date <- 0
    }
    else{
      post_date <- df_pricing[i+POST_EARNINGS_RANGE, 2]
    }
    
    # Determine earnings dates
    current_date <- df_pricing[i,]$Date
    announced <- FALSE
    if( length(earnings_dates) > e ){
      if( current_date == earnings_dates[e] ){
        announced <- TRUE
        e <- e + 1
      }
    }
    
    # Determine trade execution status
    if(i == 1){
      trade_executed <- TRUE
    }
    else if(i > 1){
      # Days where earnings were announced
      if (trade_executed && announced) {
        trade_executed <- FALSE
        buy_back_date <- post_date
      }
      # Days between the sell date and the post-earnings zone date
      else if(!trade_executed && !announced){
        if (buy_back_date == current_date){
          trade_executed <- TRUE
          buy_back_date <- NULL
          buy_price <- df_pricing[i,]$Open
        }
      }
    }
    
    trade_status <- c(trade_status, trade_executed)
    earnings_announced <- c(earnings_announced, announced)
    post_dates <- c(post_dates, post_date)
  }
  df_pricing <- df_pricing %>% mutate(EarningsDate = earnings_announced)
  post_dates <- as.Date(post_dates, origin="1970-01-01")
  df_pricing <- df_pricing %>% mutate(PostDate = post_dates)
  df_pricing <- df_pricing %>% mutate(TradeStatus = trade_status)
  
 
  investment <- 1
  account <- c()
  returns <- c()
  for(j in seq(from=1, to=nrow(df_pricing))){
    trade_status <- df_pricing[j,]$TradeStatus
    isEarnings <- df_pricing[j,]$EarningsDate
    postDate <- df_pricing[j,]$PostDate
    
    # Holding the stock
    if(!isEarnings && trade_status){
      if(j==1){
        buyPrice <- df_pricing[j,]$Open
      }
      else if(!df_pricing[j-1,]$TradeStatus){
        buyPrice <- df_pricing[j,]$Close
      }
    }
    # Earnings Day
    if(isEarnings){
      sellPrice <- df_pricing[j-1,]$Close
    }
    else if(j==nrow(df_pricing)){
      sellPrice <- df_pricing[j,]$Close
    }
    
    # Actual Returns
    if(isEarnings || j==nrow(df_pricing)){
      return <- 1 + ((sellPrice - buyPrice) / buyPrice)
    }
    else{
      return <- 1
    }
    
    if(i==1){
      investment <- 1
    }
    else{
      investment <- return*investment
    }
    
    
    returns <- c(returns, return)
    account <- c(account, investment)
  }
  
  df_pricing <- df_pricing %>% mutate(Returns = returns)
  df_pricing <- df_pricing %>% mutate(Account = account)

  stddev <- sd(returns)
  max_dd <- mdd(returns)
  earnings_strategy_df[nrow(earnings_strategy_df)+1,] <- c(current_ticker, investment, max_dd, stddev)
}

earnings_strategy_df <- earnings_strategy_df %>% distinct() %>% arrange(Ticker)

barplot(as.double(earnings_strategy_df$Return)-1, main=expression(paste("Earnings Strategy Returns")),names.arg=earnings_strategy_df$Ticker, las=2, cex.names=0.5, col=ifelse(earnings_strategy_df$Return<1,"red","lightgreen"))
mean(as.double(earnings_strategy_df$Return))

barplot(as.double(earnings_strategy_df$StandardDev), main="Earnings Strategy Standard Deviation", names.arg=earnings_strategy_df$Ticker, las=2, cex.names=0.5, col="lightblue")
mean(as.double(earnings_strategy_df$StandardDev))

barplot(as.double(earnings_strategy_df$MaxDrawdown), main="Earnings Strategy Max Drawdowns", names.arg=earnings_strategy_df$Ticker, las=2, cex.names=0.5, col="orange")
mean(as.double(earnings_strategy_df$MaxDrawdown))






