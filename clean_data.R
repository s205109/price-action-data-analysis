#
# Author: Dominic Sciarrino
# Date: 10/5/2021

install.packages("reader")
library("reader")
library("stringr")

#
# Define the path to the data folder
#
path <- paste(getwd(), "/ToS_5Y-1d_DOW30", sep="")
path

#
# Define the file to read
#

input_file = paste(path, "/StrategyReports_AMGN_91621.csv", sep="")
output_csv_file = paste(path, "/AMGN.csv", sep="")
lines_in_file = length(readLines(input_file))

#
# PRE-PROCESSING STEP 1: SKIP HEADER META-DATA
# 
# Each file has 6 rows of meta data at the beginning and 7 rows of metadata
# at the end. The data we need is in between, so when we read the lines of the
# data file, we need to skip the lines with meta data.
#
lines_to_skip = 6
lines_to_skip_at_end = 7

lines_from_file <- n.readLines(input_file,
            header = FALSE,
            skip = lines_to_skip,
            n = lines_in_file - lines_to_skip - lines_to_skip_at_end)

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
    line_date = line_data[6]
    #line_date  = as.Date(line_data[6], format = "%m/%d/%y")  # <- keep for later
    
    df[nrow(df)+1,] = list(ticker, line_date, open, high, low, close)
  }
}

write.csv(df, output_csv_file, row.names = FALSE)
