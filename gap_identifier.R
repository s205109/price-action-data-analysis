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

# Test Stock
# Define the stock and read the corresponding CSV
AAPL <- read.csv(paste(DOW30_5Y1d_data_proc_path, DOW30_5Y1d_files[1], sep="/"))
df <- tail(AAPL, 200)
df <- AAPL
df


###############################################################################
#                      SET GAP LENGTH OF CANDLESTICK                          #
###############################################################################

gap_length <- c()
gap_filled <- c()
swing <- c()

for (i in seq(from=1, to=nrow(df))) {
  
  #####################################
  #       CALCULATE GAP LENGTH        #
  #####################################
  gap = 0
  if (i == 1){
    gap <- 0
  }
  # If the opening price of candle 'i' is outside the total price swing of 
  # day 'i-1' then a gap is present between the two candlesticks.
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
  #            GAP FILLED             #
  #####################################
  if (i == 1) {
    gap_filled_bool <- FALSE
    swing <- c(swing, 0)
  }
  else if (gap == 0){
    gap_filled_bool <- FALSE
    swing <- c(swing, 0)
  }
  else if (gap > 0) {
    # If we gapped up, then check if the price ever came back down to the 
    # previous day's high
    #   1) Calculate the lower range of today's candle
    lower_swing <- df[i,]$Open - df[i,]$Low
    #   2) Check if the lower_swing is larger than the gap up.
    gap_filled_bool <- (lower_swing > gap)
    
    swing <- c(swing, lower_swing)
  }
  else if(gap < 0) {
    # If we gapped down, then check if the price ever came back up to the 
    # previous day's low
    #   1) Calculate the upper range of today's candle
    upward_swing <- df[i,]$High - df[i,]$Open
    #   2) Check if the upward_swing is larger than the gap down
    gap_filled_bool <- upward_swing > abs(gap)
    
    swing <- c(swing, upward_swing)
  }
  
  gap_filled <- c(gap_filled, gap_filled_bool)
}

df$gap_length <- gap_length
df$swing <- swing
df$gap_filled <- gap_filled

df

num_of_gaps <- nrow(subset(df, gap_length != 0))
num_of_gaps_filled <- nrow(subset(df, gap_filled == TRUE))
ratio_gaps_filled <- num_of_gaps_filled / num_of_gaps

num_of_gaps
num_of_gaps_filled
ratio_gaps_filled

d <- df %>% filter(gap_length != 0)
d
s <- d %>% group_by(gap_filled) %>% summarize(counts = n(), percentage=n()/nrow(d))
s

colors <- c("red", "green")
labels <- c("Gap Not Filled", "Gap Filled")
gap_pie_chart <- ggplot(data=s, aes(x="", y=percentage, fill = colors)) + 
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
  ggtitle("Number of Gaps Filled in AAPL Stock Over 3 Years") + 
  scale_fill_manual(values = labels)

gap_pie_chart

gap_pie_chart2 <- plot_ly(data=s, labels= ~labels, values = ~percentage,
                          type = 'pie', sort=FALSE,
                          marker= list(colors=colors, line=list(color="black", width=1))) %>%
  layout(title="Number of Gaps in AAPL Stock Over 3 Years")

gap_pie_chart2
