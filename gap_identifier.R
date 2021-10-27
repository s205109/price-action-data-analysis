setwd("/Users/dominicsciarrino/Documents/projects/trading/price-action-data-analysis")

library(dplyr)
library(ggplot2)
library(plotly)

# Author: Dominic Sciarrino
# Date: 10/26/2021

#
# Define the path to the processed data folder
#
data_processed_path <- paste(getwd(), "/data/processed", sep="")
data_processed_path

# Processed data for DOW 30 5Y 1d data
DOW30_5Y1d_data_proc_path <- paste(data_processed_path, "/5Y-1d_DOW30", sep="")

# List the files for the DOW 30 files
DOW30_5Y1d_files <- list.files(
  path = DOW30_5Y1d_data_proc_path
)

# Data Frame for holding gap information for the DOW 30 stocks
gap_info_df <- data.frame(Ticker=character(),
                          Gap_Fill_Percentage=double())

# Iterate through stock CSV files
for(j in seq(from=1, to=length(DOW30_5Y1d_files))){
  df <- read.csv(paste(DOW30_5Y1d_data_proc_path, DOW30_5Y1d_files[j], sep="/"))
  
  ####################################################################
  #                SET GAP LENGTH OF CANDLESTICK                     #
  ####################################################################
  gap_length <- c()                   # Size of the gap from day i-1 to day i
  gap_filled <- c()                   # A bool flag if gap was filled on day i
  swing <- c()                        # Size of price movement in direction of 
                                      #    gap fill on day i
  
  # 
  for (i in seq(from=1, to=nrow(df))) {
    
    #####################################
    #       CALCULATE GAP LENGTH        #
    #####################################
    gap <- 0
    if(i == 1){
      gap <- 0
    }
    # GAP DEFINITION:
    #
    # If the opening price of candle 'i' is outside the total price swing of 
    # day 'i-1' then a gap is present between the two candlesticks.
    #
    # The gap is calculated as the Open of today minus the Close of the 
    # previous day.
    else if (df[i-1,]$High < df[i,]$Open){
      gap <- df[i,]$Open - df[i-1,]$High # Gap Up
    }
    else if (df[i-1,]$Low > df[i,]$Open){
      gap <- df[i,]$Open - df[i-1,]$Low # Gap Down (negative)
    }
    gap_length <- c(gap_length, gap)
    
    
    #####################################
    #       CHECK IF GAP FILLED         #
    #####################################
    if (i == 1) {
      gap_filled_bool <- FALSE
      swing <- c(swing, 0)
    }
    else if (gap == 0){
      gap_filled_bool <- FALSE
      swing <- c(swing, 0)
    }
    # GAP UP:
    #
    # Check if the price ever came back down to the previous day's high
    else if (gap > 0) {
      # 1) Calculate the lower range of today's candle
      lower_swing <- df[i,]$Open - df[i,]$Low
      # 2) Check if the lower_swing is larger than the gap up.
      gap_filled_bool <- (lower_swing > gap)
      swing <- c(swing, lower_swing)
    }
    # GAP DOWN:
    #
    # Check if the price ever came back up to the previous day's low
    else if(gap < 0) {
      # 1) Calculate the upper range of today's candle
      upward_swing <- df[i,]$High - df[i,]$Open
      # 2) Check if the upward_swing is larger than the gap down
      gap_filled_bool <- upward_swing > abs(gap)
      
      swing <- c(swing, upward_swing)
    }
    
    gap_filled <- c(gap_filled, gap_filled_bool)
  }
  
  ###########################################
  #     UPDATE CURRENT STOCK DATA FRAME     #
  ###########################################
  df$gap_length <- gap_length
  df$swing <- swing
  df$gap_filled <- gap_filled
  df
  
  ############################
  # GAPS FILLED / TOTAL GAPS #
  ############################
  d <- df %>% filter(gap_length != 0)
  s <- d %>% group_by(gap_filled) %>% summarize(counts = n(), percentage = n()/nrow(d))
  gaps_filled_percentage <- (s %>% filter(gap_filled == TRUE))$percentage
  
  gap_info_df[nrow(gap_info_df)+1,] <- c(df[1,]$Ticker, gaps_filled_percentage[1])
}


###################################################
#      Top 10 DOW 30 Stocks that Gap Filled       #
###################################################
top10_gap_fillers <- arrange(gap_info_df, desc(Gap_Fill_Percentage)) %>% slice(1:10)
top10_gap_fillers_barplot <- ggplot(data=top10_gap_fillers, aes(x=Ticker, y=Gap_Fill_Percentage)) + 
  geom_bar(stat="identity", fill="steelblue") +
  labs(y="% of Gaps Filled", x="Ticker") +
  labs(title="Top 10 Gap Fillers in the DOW 30")
top10_gap_fillers_barplot



#############################
#   PIE CHART FOR 1 STOCK   # ggplot2
#############################
colors <- c("green", "red")
labels <- c("Gap Not Filled", "Gap Filled")
gap_pie_chart <- ggplot(data=s, aes(x="", y=percentage, fill = labels)) + 
  geom_col(color="black") +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(round(percentage*100), "%")),
            position = position_stack(vjust = 0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 10)) +
  ggtitle("Number of Gaps Filled Within 1 Trading Day (AAPL, 3 Years)") + 
  scale_fill_manual(values = colors)

gap_pie_chart

#############################
#   PIE CHART FOR 1 STOCK   # plot_ly
#############################
gap_pie_chart2 <- plot_ly(data=s, labels= ~labels, values = ~percentage,
                          type = 'pie', sort=FALSE,
                          marker= list(colors=colors, line=list(color="black", width=1))) %>%
  layout(title="Number of Gaps in AAPL Stock Over 3 Years")

gap_pie_chart2
