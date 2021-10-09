setwd("/Users/dominicsciarrino/Documents/projects/trading/price-action-data-analysis")

# Author: Dominic Sciarrino
# Date: 10/5/2021

install.packages("reader")
library("reader")
library("stringr")
library("lubridate")

#
# Define the path to the data folder
#
data_store_path <- paste(getwd(), "/data/store", sep="")
data_store_path

#
# List the available files for each timeframe and for each exchange
# in the data store.
#
DOW30_5Y1d_data_store_path <- paste(data_store_path, "/ToS_5Y-1d_DOW30", sep="")
DOW30_5Y1d_files <- list.files(
  path = DOW30_5Y1d_data_store_path
)

DOW30_3M5m_data_store_path <- paste(data_store_path, "/ToS_3M-5m_DOW30", sep="")
DOW30_3M5m_files <- list.files(
  path = DOW30_3M5m_data_store_path
)

NASDAQ_5Y1d_data_store_path <- paste(data_store_path, "/ToS_5Y-1d_NASDAQ100", sep="")
NASDAQ_5Y1d_files <- list.files(
  path = NASDAQ_5Y1d_data_store_path
)

NASDAQ_3M5m_data_store_path <- paste(data_store_path, "/ToS_3M-5m_NASDAQ100", sep="")
NASDAQ_3M5m_files <- list.files(
  path = NASDAQ_3M5m_data_store_path
)


data_processed_path <- paste(getwd(), "/data/processed", sep="")
data_processed_path

DOW30_5Y1d_data_proc_path <- paste(data_processed_path, "/5Y-1d_DOW30", sep="")
DOW30_3M5m_data_proc_path <- paste(data_processed_path, "/3M-5m_DOW30", sep="")
NASDAQ_5Y1d_data_proc_path <- paste(data_processed_path, "/5Y-1d_NASDAQ100", sep="")
NASDAQ_3M5m_data_proc_path <- paste(data_processed_path, "/3M-5m_NASDAQ100", sep="")


#
# Define which file to read
#
for (fd in DOW30_5Y1d_files) {
  # Input File Path
  input_file_path <- paste(DOW30_5Y1d_data_store_path, fd, sep="/")
  
  # Output File Path
  fd_ticker <- str_split_fixed(fd, pattern="_", n = 3)[,2]
  output_file_path <- paste(DOW30_5Y1d_data_proc_path, paste(fd_ticker, ".csv", sep=""), sep="/")

  #
  # PRE-PROCESSING STEP 1: SKIP HEADER META-DATA
  # 
  # Each file has 6 rows of meta data at the beginning and 7 rows of metadata
  # at the end. The data we need is in between, so when we read the lines of the
  # data file, we need to skip the lines with meta data.
  #
  lines_to_skip = 6
  lines_to_skip_at_end = 7
  lines_in_input_file <- length(readLines(input_file_path))
  
  lines_from_file <- n.readLines(input_file_path,
                                 header = FALSE,
                                 skip = lines_to_skip,
                                 n = lines_in_input_file - lines_to_skip - lines_to_skip_at_end)
  
  #
  # PRE-PROCESSING STEP 2: EXTRACT USEFUL DATA
  # 
  # We're only interested in price data, the date/time of that data, and the 
  # associated stock ticker symbol.
  #
  
  # Set up columns
  df <- data.frame(matrix(ncol=6, nrow=0, dimnames=list(NULL, c("Ticker", "Date", "Open", "High", "Low", "Close"))))
  
  for (line in lines_from_file) {
    # split the string based on a semi-colon delimiter
    line_data = str_split(line, pattern = ";", simplify=TRUE)

    # the data is duplicated in the text files, so only consider the 
    # data with the "Buy to Open" flag, instead of "Sell to Close"
    if (line_data[3] == "Buy to Open") {
      # Get the string that has the pricing data and stock ticker symbol
      raw_ohlcp = str_split(line_data[2], pattern="\\|", simplify=TRUE)
      ticker = raw_ohlcp[2]
      open = as.double(raw_ohlcp[3])
      high = as.double(raw_ohlcp[4])
      low  = as.double(raw_ohlcp[5])
      close = as.double(raw_ohlcp[6])
      
      # Get the associated date of the pricing data
      # Change format so I can use lubridate later for analysis/processing
      line_date <- str_replace_all(line_data[6], "/", "-")
      #mdy(line_date) -> returns dataframe
      #month = month(line_date)
      #year = year(line_date)
      #day = mday(line_date)
      
      df[nrow(df)+1,] = list(ticker, line_date, open, high, low, close)
    }
    
    # Write the output file
    write.csv(df, output_file_path, row.names = FALSE)
  }
}





for (fd in DOW30_3M5m_files) {
  input_file_path <- paste(DOW30_3M5m_data_store_path, fd, sep="/")
  fd_ticker <- str_split_fixed(fd, pattern="_", n = 3)[,2]
  output_file_path <- paste(DOW30_3M5m_data_proc_path, paste(fd_ticker, ".csv", sep=""), sep="/")
  
  lines_to_skip = 6
  lines_to_skip_at_end = 7
  lines_in_input_file <- length(readLines(input_file_path))
  lines_from_file <- n.readLines(input_file_path,
                                 header = FALSE,
                                 skip = lines_to_skip,
                                 n = lines_in_input_file - lines_to_skip - lines_to_skip_at_end)
  
  df <- data.frame(matrix(ncol=6, nrow=0, dimnames=list(NULL, c("Ticker", "Date", "Open", "High", "Low", "Close"))))
  
  for (line in lines_from_file) {
    line_data = str_split(line, pattern = ";", simplify=TRUE)
    
    if (line_data[3] == "Buy to Open") {
      raw_ohlcp = str_split(line_data[2], pattern="\\|", simplify=TRUE)
      ticker = raw_ohlcp[2]
      open = as.double(raw_ohlcp[3])
      high = as.double(raw_ohlcp[4])
      low  = as.double(raw_ohlcp[5])
      close = as.double(raw_ohlcp[6])
      
      line_date <- str_replace_all(line_data[6], "/", "-")
      
      df[nrow(df)+1,] = list(ticker, line_date, open, high, low, close)
    }
    
    write.csv(df, output_file_path, row.names = FALSE)
  }
}





for (fd in NASDAQ_5Y1d_files) {
  input_file_path <- paste(NASDAQ_5Y1d_data_store_path, fd, sep="/")
  fd_ticker <- str_split_fixed(fd, pattern="_", n = 3)[,2]
  output_file_path <- paste(NASDAQ_5Y1d_data_proc_path, paste(fd_ticker, ".csv", sep=""), sep="/")
  
  lines_to_skip = 6
  lines_to_skip_at_end = 7
  lines_in_input_file <- length(readLines(input_file_path))
  lines_from_file <- n.readLines(input_file_path,
                                 header = FALSE,
                                 skip = lines_to_skip,
                                 n = lines_in_input_file - lines_to_skip - lines_to_skip_at_end)
  
  df <- data.frame(matrix(ncol=6, nrow=0, dimnames=list(NULL, c("Ticker", "Date", "Open", "High", "Low", "Close"))))
  
  for (line in lines_from_file) {
    line_data = str_split(line, pattern = ";", simplify=TRUE)
    
    if (line_data[3] == "Buy to Open") {
      raw_ohlcp = str_split(line_data[2], pattern="\\|", simplify=TRUE)
      ticker = raw_ohlcp[2]
      open = as.double(raw_ohlcp[3])
      high = as.double(raw_ohlcp[4])
      low  = as.double(raw_ohlcp[5])
      close = as.double(raw_ohlcp[6])
      
      line_date <- str_replace_all(line_data[6], "/", "-")
      
      df[nrow(df)+1,] = list(ticker, line_date, open, high, low, close)
    }
    
    write.csv(df, output_file_path, row.names = FALSE)
  }
}









for (fd in NASDAQ_3M5m_files) {
  input_file_path <- paste(NASDAQ_3M5m_data_store_path, fd, sep="/")
  fd_ticker <- str_split_fixed(fd, pattern="_", n = 3)[,2]
  output_file_path <- paste(NASDAQ_3M5m_data_proc_path, paste(fd_ticker, ".csv", sep=""), sep="/")
  
  lines_to_skip = 6
  lines_to_skip_at_end = 7
  lines_in_input_file <- length(readLines(input_file_path))
  lines_from_file <- n.readLines(input_file_path,
                                 header = FALSE,
                                 skip = lines_to_skip,
                                 n = lines_in_input_file - lines_to_skip - lines_to_skip_at_end)
  
  df <- data.frame(matrix(ncol=6, nrow=0, dimnames=list(NULL, c("Ticker", "Date", "Open", "High", "Low", "Close"))))
  
  for (line in lines_from_file) {
    line_data = str_split(line, pattern = ";", simplify=TRUE)
    
    if (line_data[3] == "Buy to Open") {
      raw_ohlcp = str_split(line_data[2], pattern="\\|", simplify=TRUE)
      ticker = raw_ohlcp[2]
      open = as.double(raw_ohlcp[3])
      high = as.double(raw_ohlcp[4])
      low  = as.double(raw_ohlcp[5])
      close = as.double(raw_ohlcp[6])
      
      line_date <- str_replace_all(line_data[6], "/", "-")
      
      df[nrow(df)+1,] = list(ticker, line_date, open, high, low, close)
    }
    
    write.csv(df, output_file_path, row.names = FALSE)
  }
}
