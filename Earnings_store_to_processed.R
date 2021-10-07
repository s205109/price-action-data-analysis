setwd("/Users/dominicsciarrino/Documents/projects/trading/price-action-data-analysis")

# Author: Dominic Sciarrino
# Date: 10/7/2021

install.packages("reader")
library("reader")
library("stringr")
library("lubridate")

#
# Define the path to the data store folder
#
data_store_path <- paste(getwd(), "/data/store", sep="")
data_store_path

#
# List the available files for each timeframe and for each exchange
# in the data store.
#
DOW30_Earnings_data_store_path <- paste(data_store_path, "/ToS_Earnings_DOW30", sep="")
DOW30_Earnings_files <- list.files(
  path = DOW30_Earnings_data_store_path
)

NASDAQ100_Earnings_data_store_path <- paste(data_store_path, "/ToS_Earnings_NASDAQ100", sep="")
NASDAQ100_Earnings_files <- list.files(
  path = NASDAQ100_Earnings_data_store_path
)

DOW30_Earnings_files
NASDAQ100_Earnings_files

#
# Define the path to the processed folder
#
data_processed_path <- paste(getwd(), "/data/processed", sep="")
data_processed_path

DOW30_Earnings_data_proc_path <- paste(data_processed_path, "/DOW30_Earnings", sep="")
NASDAQ_Earnings_data_proc_path <- paste(data_processed_path, "/NASDAQ100_Earnings", sep="")

DOW30_Earnings_data_proc_path
NASDAQ_Earnings_data_proc_path

# Set up dataframes
df_DOW30 <- data.frame(matrix(ncol=5, nrow=0, dimnames=list(NULL, c("Ticker", "Date", "Announced", "AEPS", "EEPS"))))
df_NASDAQ100 <- data.frame(matrix(ncol=5, nrow=0, dimnames=list(NULL, c("Ticker", "Date", "Announced", "AEPS", "EEPS"))))

DOW30_output_path <- paste(DOW30_Earnings_data_proc_path, "DOW30_Earnings.csv", sep="/")
NASDAQ100_output_path <- paste(NASDAQ_Earnings_data_proc_path, "NASDAQ100_Earnings.csv", sep="/")

DOW30_output_path
NASDAQ100_output_path

#############################
#   DOW 30 Earnings Files   #
#############################
for (fd in DOW30_Earnings_files) {
  # Input File Path
  input_file_path <- paste(DOW30_Earnings_data_store_path, fd, sep="/")
  
  lines_to_skip = 6
  lines_to_skip_at_end = 7
  lines_in_input_file <- length(readLines(input_file_path))
  
  lines_from_file <- n.readLines(input_file_path,
                                 header = FALSE,
                                 skip = lines_to_skip,
                                 n = lines_in_input_file - lines_to_skip - lines_to_skip_at_end)
  
  for (line in lines_from_file) {
    # split the string based on a semi-colon delimiter
    line_data <- str_split(line, pattern = ";", simplify=TRUE)
    
    # the data is duplicated in the text files, so only consider the 
    # data with the "Buy to Open" flag, instead of "Sell to Close"
    if (line_data[3] == "Buy to Open") {
      # Split line containing raw earnings data
      raw_earnings_data <- str_split(line_data[2], pattern="\\|", simplify=TRUE)
      
      # Ticker Symbol
      ticker <- str_split(raw_earnings_data[1], pattern="\\(", simplify=TRUE)[2]
      # Earnings Announcement Time
      announced <- "NaN"
      if (raw_earnings_data[2] == "1"){
        announced <- "Before"
      }
      if (raw_earnings_data[3] == "1"){
        announced <- "After"
      }
      # Actual Earnings Per Share (EPS)
      actual_EPS <- as.numeric(raw_earnings_data[4])
      # Expected Earnings Per Share (EPS) (first remove the ')')
      expected_EPS <- as.numeric(str_split(raw_earnings_data[5], pattern="\\)", simplify=TRUE)[1])
      # Earnings Date
      line_date <- str_replace_all(line_data[6], "/", "-")
      
      df_DOW30[nrow(df_DOW30)+1,] <- list(ticker, line_date, announced, actual_EPS, expected_EPS)
    }
  }
}

# Write the DOW30 output file
write.csv(df_DOW30, DOW30_output_path, row.names = FALSE)

#############################
# NASDAQ 100 Earnings Files #
#############################
for (fd in NASDAQ100_Earnings_files) {
  # Input File Path
  input_file_path <- paste(NASDAQ100_Earnings_data_store_path, fd, sep="/")

  lines_to_skip = 6
  lines_to_skip_at_end = 7
  lines_in_input_file <- length(readLines(input_file_path))
  
  lines_from_file <- n.readLines(input_file_path,
                                 header = FALSE,
                                 skip = lines_to_skip,
                                 n = lines_in_input_file - lines_to_skip - lines_to_skip_at_end)
  for (line in lines_from_file) {
    # split the string based on a semi-colon delimiter
    line_data <- str_split(line, pattern = ";", simplify=TRUE)
    
    # the data is duplicated in the text files, so only consider the 
    # data with the "Buy to Open" flag, instead of "Sell to Close"
    if (line_data[3] == "Buy to Open") {
      # Split line containing raw earnings data
      raw_earnings_data <- str_split(line_data[2], pattern="\\|", simplify=TRUE)
      
      # Ticker Symbol
      ticker <- str_split(raw_earnings_data[1], pattern="\\(", simplify=TRUE)[2]
      # Earnings Announcement Time
      announced <- "NaN"
      if (raw_earnings_data[2] == "1"){
        announced <- "Before"
      }
      if (raw_earnings_data[3] == "1"){
        announced <- "After"
      }
      # Actual Earnings Per Share (EPS)
      actual_EPS <- as.numeric(raw_earnings_data[4])
      # Expected Earnings Per Share (EPS) (first remove the ')')
      expected_EPS <- as.numeric(str_split(raw_earnings_data[5], pattern="\\)", simplify=TRUE)[1])
      # Earnings Date
      line_date <- str_replace_all(line_data[6], "/", "-")
      
      df_NASDAQ100[nrow(df_NASDAQ100)+1,] <- list(ticker, line_date, announced, actual_EPS, expected_EPS)
    }
  }
}

# Write the NASDAQ 100 output file
write.csv(df_NASDAQ100, NASDAQ100_output_path, row.names = FALSE)

