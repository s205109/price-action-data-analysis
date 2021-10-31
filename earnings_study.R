setwd("/Users/dominicsciarrino/Documents/projects/trading/price-action-data-analysis")

library(dplyr)
library(ggplot2)
library(plotly)

# Author: Dominic Sciarrino
# Date: 10/28/2021

# Constants
PRE_EARNINGS_RANGE <- 8
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

for(k in seq(from=1, to=length(DOW30_5Y1d_files))){
  # Get Pricing Data for current stock
  df_pricing <- read.csv(paste(DOW30_5Y1d_data_proc_path, DOW30_5Y1d_files[k], sep="/"))
  df_pricing$Date <- as.Date(df_pricing$Date, format="%m-%d-%y")   ## Format to Date class 
  current_ticker <- df_pricing$Ticker[1]  ## Get the ticker of current stock
  
  # Filter the earnings data so only the current stock is considered
  current_earnings_data <- df_earnings %>% filter(Ticker == current_ticker)
  current_earnings_data$Date <- as.Date(current_earnings_data$Date, format="%m-%d-%y", origin="01-01-70")
  
  # Calculate daily returns
  returns <- c(0)
  for(i in seq(from=1, to=nrow(df_pricing))){
    return <- (df_pricing[i,]$Close - df_pricing[i-1,]$Close) / df_pricing[i-1,]$Close
    returns <- c(returns, return)
  }
  df_pricing <- df_pricing %>% mutate(Return = returns)
  
  # Merge the Earnings Data and the Pricing Data into one data frame
  df_pricing <- merge(df_pricing, current_earnings_data, by=c("Date", "Ticker"), all=TRUE)
  
  # Replace NAs with zeroes in the Earnings Price Per Share columns
  df_pricing$AEPS[is.na(df_pricing$AEPS)] <- 0
  df_pricing$EEPS[is.na(df_pricing$EEPS)] <- 0
  
  # Deselect Announced and irrelevant pricing columns
  df_pricing <- df_pricing %>% select(-Announced, -Open, -High, -Low)
  # Add Column for Logical Beating of Expectations
  df_pricing <- df_pricing %>% mutate(Beat_Expected = (AEPS > EEPS))
  
  # Add Pre-Earnings Zone Start Date
  pre_dates <- c()
  for(i in seq(from=1, to=nrow(df_pricing))){
    if(i <= PRE_EARNINGS_RANGE){
      pre_dates <- c(pre_dates, 0)
    }
    else{
      pre_dates <- c(pre_dates, df_pricing[i - PRE_EARNINGS_RANGE, 1])
    }
  }
  pre_dates <- as.Date(pre_dates, origin="1970-01-01")
  df_pricing <- df_pricing %>% mutate(PreDate = pre_dates)
  
  # Add Post-Earnings Zone End Date
  post_dates <- c()
  for(i in seq(from=1, to=nrow(df_pricing))){
    if( (i + POST_EARNINGS_RANGE) >= nrow(df_pricing) ){
      post_dates <- c(post_dates, 0)
    }
    else{
      post_dates <- c(post_dates, df_pricing[i+POST_EARNINGS_RANGE, 1])
    }
  }
  post_dates <- as.Date(post_dates, origin="1970-01-01")
  df_pricing <- df_pricing %>% mutate(PostDate = post_dates)
  
  df_pricing <- df_pricing %>% mutate(row_number= 1:n())
  
  first_date_threshold <- df_pricing[PRE_EARNINGS_RANGE,]$Date
  last_date_threshold <- df_pricing[nrow(df_pricing) - POST_EARNINGS_RANGE,]$Date
  pre_returns <- c()
  post_returns <- c()
  for(i in seq(from=1, to=nrow(current_earnings_data))){
    d <- current_earnings_data[i,]$Date
    d_pre <- (df_pricing %>% filter(Date == d) %>% select(PreDate))[1,]
    d_post <- (df_pricing %>% filter(Date == d) %>% select(PostDate))[1,]
    
    d_current_idx <- (df_pricing %>% filter(Date == d) %>% select(row_number))[1,]
    
    # Calculate the pre-earnings rate of return
    # from PRE_EARNINGS_RANGE days prior to today-1
    if(d_pre != "1970-01-01" && d_pre >= first_date_threshold){
      d_pre_row_idx <- (df_pricing %>% filter(Date == d_pre) %>% select(row_number))[1,]
      pre_ret_total <- 1
      for(j in d_pre_row_idx:(d_current_idx-1)){
        pre_ret_total <- (1 + (df_pricing %>% filter(row_number == j) %>% select(Return))[1,]) * pre_ret_total
      }
      
      # Calculate the post-earnings rate of return
      # from today + 1 to POST_EARNINGS_RANGE after
      if(d_post != "1970-01-01" && d_post <= last_date_threshold){
        d_post_row_idx <- (df_pricing %>% filter(Date == d_post) %>% select(row_number))[1,]
        post_ret_total <- 1
        for(j in (d_current_idx+1):d_post_row_idx){
          post_ret_total <- (1 + (df_pricing %>% filter(row_number == j) %>% select(Return))[1,]) * post_ret_total
        }
        
        pre_returns <- c(pre_returns, pre_ret_total)
        post_returns <- c(post_returns, post_ret_total)
      }
      else{
        pre_returns <- c(pre_returns, NA)
        post_returns <- c(post_returns, NA)
      }
    }
    
  }

  # Add Pre Returns to the Earnings Datafame
  current_earnings_data <- current_earnings_data %>% mutate(Pre_Returns = pre_returns)
  
  # Add Post Returns to the Earnings Dataframe
  current_earnings_data <- current_earnings_data %>% mutate(Post_Returns = post_returns)
  
  current_earnings_data[is.na(current_earnings_data)] <- 1

  p <- boxplot(current_earnings_data %>% select(Pre_Returns, Post_Returns), ylab="Returns", main=paste("Statistical Returns Pre- and Post-Earnings Day for ", (current_earnings_data %>% select(Ticker))[1,]))
}

current_earnings_data

