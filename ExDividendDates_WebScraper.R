setwd("/Users/dominicsciarrino/Documents/projects/trading/price-action-data-analysis")

# Author: Dominic Sciarrino
# Date: 10/5/2021

install.packages(rvest)
library(rvest)
library(lubridate)

# Setup Data Storage Directory
data_store_path <- paste(getwd(), "/data/store/Fidelity_Dividend_Dates", sep="")
data_store_path

# Get Relevant Date Information
startDate <- mdy("9/19/2016")
endDate <- mdy("9/16/2021")
start_year <- year(startDate)
end_year <- year(endDate)


# Yearly Loop
for (y in start_year:end_year){
  initial_date = mdy(paste("1/1/", y, sep=""))
  terminating_date <- mdy(paste("12/31/", y, sep=""))
  if(y == start_year){
    initial_date <- startDate
  }
  if(y == end_year){
    terminating_date <- endDate
  }
  
  title <- c("Ticker", "Dividend", "Announcement Date", "Record Date", "Ex-Date", "Pay Date")

  for (d in 0:time_length(terminating_date - initial_date, unit="days")){
    df <- data.frame(matrix(ncol=6, nrow=0, dimnames=list(NULL, title)))
    data_store_folder_path <- paste(data_store_path, year(initial_date + d), month(initial_date + d), sep="/")
    data_store_file_path <- paste(data_store_folder_path, paste(day(initial_date + d), ".csv", sep=""), sep="/")
    date_to_address <- paste(month(initial_date + d), "/", day(initial_date + d), "/", year(initial_date + d), sep="")
    
    link <- paste("https://eresearch.fidelity.com/eresearch/conferenceCalls.jhtml?tab=dividends&begindate=", date_to_address, sep="")
    rows <- read_html(link) %>% html_nodes(xpath='//*[@id="messageTable3"]') %>% html_elements("tbody") %>% html_elements("tr")
    
    for (row in rows) {
        df[nrow(df)+1,] <- row %>% html_elements("td") %>% html_text2()
    }
    
    write.csv(df, data_store_file_path, row.names = FALSE)
  }
}

