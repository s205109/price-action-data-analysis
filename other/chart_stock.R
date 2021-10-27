setwd("/Users/dominicsciarrino/Documents/projects/trading/price-action-data-analysis")

# Author: Dominic Sciarrino
# Date: 10/12/2021

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

# Define the stock and read the corresponding CSV
AAPL <- read.csv(paste(DOW30_5Y1d_data_proc_path, DOW30_5Y1d_files[1], sep="/"))
df <- tail(AAPL, 30)
df

# Use plot.ly for candlestick chart rendering
install.packages("plotly")
library(plotly)

# Create the candlestick chart figure
fig <- df %>% plot_ly(x = ~ Date, type="candlestick",
                      open = ~df$Open, close = ~df$Close,
                      high = ~df$High, low = ~df$Low)

fig <- fig %>% layout(title = "AAPL Stock", xaxis=list(rangeslider = list(visible = F)))
fig

# annotation
a <- list(text = "Stock Split",
          x = '2021-8-25',
          y = 1.02,
          xref = 'x',
          yref = 'paper',
          xanchor = 'left',
          showarrow = FALSE
)

# use shapes to create a line
l <- list(type = line,
          x0 = '2021-8-13',
          x1 = '2021-8-26',
          y0 = 0.5,
          y1 = 5,
          xref = 'x',
          yref = 'paper',
          line = list(color = 'black',
                      width = 0.5)
)

fig <- fig %>% layout(title = "AAPL Stock", 
                      xaxis=list(rangeslider = list(visible = F)),
                      annotations = a,
                      shapes = l)
fig
