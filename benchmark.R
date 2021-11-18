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

all_files <- c()
for (f in DOW30_5Y1d_files){
  all_files <- c(all_files, paste(DOW30_5Y1d_data_proc_path, f, sep="/"))
}
for (f in NASDAQ100_5Y1d_files){
  all_files <- c(all_files, paste(NASDAQ_5Y1d_data_proc_path, f, sep="/"))
}


base_return_summary <- data.frame(Ticker=character(),
                                  TotalReturn=double(),
                                  Drawdown=double(),
                                  StandardDev=double(),
                                  Variance=double())

####################################################################
#           BENCHMARK BUY AND HOLD FOR ALL STOCKS                  #
####################################################################
for (k in seq(from=1, to=length(all_files))){
  df_pricing <- read.csv(all_files[k])

  base_returns <- c()
  total_return <- 1
  current_ticker <- df_pricing[k,]$Ticker[1]
  
  total_return <- ((df_pricing[nrow(df_pricing),]$Close - df_pricing[1,]$Open) / df_pricing[1,]$Open)
  
  for (i in seq(from=1, to=nrow(df_pricing))) {
    # Calculate Base Returns
    base_return <- 1 + ((df_pricing[i,]$Close - df_pricing[i,]$Open) / df_pricing[i,]$Open)
    base_returns <- c(base_returns, base_return)
    #total_return <- total_return * base_return
  }
  
  ###########################################
  #       UPDATE BACKTEST DATA FRAME        #
  ###########################################
  mdd_base <- mdd(base_returns)
  stddev <- sd(base_returns)
  sigma <- variance(base_returns)
  
  base_return_summary[nrow(base_return_summary)+1,] = c(current_ticker,
                                                        total_return,
                                                        mdd_base,
                                                        stddev,
                                                        sigma)
}

base_return_summary <- base_return_summary %>% distinct() %>% arrange(Ticker)
View(base_return_summary)

barplot(as.double(base_return_summary$TotalReturn), main="Benchmark Returns (Buy-and-Hold Strategy)", names.arg=base_return_summary$Ticker, las=2, cex.names=0.5, col=ifelse(base_return_summary$TotalReturn<0,"red","lightgreen"))
mean(as.double(base_return_summary$TotalReturn))

barplot(as.double(base_return_summary$StandardDev), main="Benchmark Standard Deviation (Buy-and-Hold Strategy)", names.arg=base_return_summary$Ticker, las=2, cex.names=0.5, col="lightblue")
mean(as.double(base_return_summary$StandardDev))

barplot(as.double(base_return_summary$Drawdown), main="Benchmark Max Drawdowns (Buy-and-Hold Strategy)", names.arg=base_return_summary$Ticker, las=2, cex.names=0.5, col="orange")
mean(as.double(base_return_summary$Drawdown))
