# The purpose of this file is to compile Fidelity dividend information into one
# dataframe and save this to a "master" CSV file for ex-dividend date information

setwd("/Users/dominicsciarrino/Documents/projects/trading/price-action-data-analysis")

# Author: Dominic Sciarrino
# Date: 10/5/2021

install.packages("reader")
library("reader")
library("stringr")
library("lubridate")
library("dplyr")

#
# Define the path to the data folder
#
fidelity_dates_folder <- paste(getwd(), "/data/store/Fidelity_Dividend_Dates", sep="")
fidelity_dates_folder
processed_fidelity_folder <- paste(getwd(), "/data/processed/Fidelity_Dividend_Data", sep="")

# Define the Timeframe we want to compile data for
startDate <- mdy("9/19/2016")
endDate <- mdy("9/16/2021")
start_year <- year(startDate)
end_year <- year(endDate)

# Define the dataframe and title vector
title <- c("Ticker", "Dividend", "Announcement Date", "Record Date", "Ex-Date", "Pay Date")
df <- data.frame(matrix(ncol=6, nrow=0, dimnames=list(NULL, title)))

# Yearly Loop
for (y in start_year:end_year){
  # Initial date will be the first of the starting year
  initial_date = mdy(paste("1/1/", y, sep=""))
  
  # Terminating date will be the last day of the ending year
  terminating_date <- mdy(paste("12/31/", y, sep=""))
  
  # If a different date is specified to start on, go with that
  if(y == start_year){
    initial_date <- startDate
  }
  # Same with the end date
  if(y == end_year){
    terminating_date <- endDate
  }
  
  # Iterate over the dates in the year
  for (i in 0:time_length(terminating_date - initial_date, unit="days")){
    # Get the current date
    current_date <- i + initial_date
    
    # Set up the source file name
    m <- month(current_date)
    d <- day(current_date)
    src_file <- paste(fidelity_dates_folder, y, m, paste(d, ".csv", sep=""), sep="/")
    
    # Read the CSV file at the file location
    csv_fd <- read.csv(src_file)

    # Bind the data in the CSV file to the current dataframe object
    df <- rbind(df, csv_fd)
    
  }
}

# Examine contents
head(df)
tail(df)

# Remove the days where no dividend was issued (like weekends and holidays)
dividend_data <- df[df$Ticker != "No Dividends for this date", ]

# Arrange the data alphabetically by Ticker name
arranged_dividend_data <- arrange(dividend_data, dividend_data$Ticker, dividend_data$Ex.Date)

# Write it to a CSV file
write.csv(arranged_dividend_data, paste(processed_fidelity_folder, "Fidelity_Dividend_Data.csv", sep="/"), row.names = FALSE)

