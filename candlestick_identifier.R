setwd("/Users/dominicsciarrino/Documents/projects/trading/price-action-data-analysis")

# Author: Dominic Sciarrino
# Date: 10/18/2021

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
df

###############################################################################
#               SET DIRECTION AND PRICE RANGE OF CANDLESTICK                  #
###############################################################################
price_direction = c()
price_range = c()
body_size = c()
body_average = c()
mid_point = c()
for(i in seq(from=1, to=nrow(df))){
  direction = "NONE"
  r = 0
  b = 0
  b_avg = 0
  m = 0
  if (is.nan(df[i,]$Open) || is.nan(df[i,]$Close)){
    direction = NaN
    r = 0
    b = 0
    b_avg = 0
    m = 0
  }
  
  # Set Direction
  if (df[i,]$Open < df[i,]$Close) {
    direction = "UP"
    b = df[i,]$Close - df[i,]$Open
    b_avg = (b/2) + df[i,]$Open
  }
  if (df[i,]$Open > df[i,]$Close) {
    direction = "DOWN"
    b = df[i,]$Open - df[i,]$Close
    b_avg = (b/2) + df[i,]$Close
  }
  price_direction = c(price_direction,direction)
  
  # Set price range
  r = df[i,]$High - df[i,]$Low
  price_range = c(price_range, r)
  
  # Set body size
  body_size = c(body_size, b)
  
  # Set average body price
  body_average = c(body_average, b_avg)
  
  # Set midpoint/mid-price
  m = (df[i,]$High + df[i,]$Low) / 2
  mid_point = c(mid_point, m)
}
df$direction = price_direction
df$range = price_range
df$body = body_size
df$body_average = body_average
df$mid_point = mid_point
df

###############################################################################
#                         SET TYPES OF CANDLESTICKS                           #
###############################################################################

# ERROR & THRESHOLD PARAMETERS
MARUBOZU_ERROR <- 0.07          # 7%
MARUBOZU_THRESHOLD <- 0.80      # 80%
DOJI_ERROR <- 0.07              # 7%
SPINNER_ERROR <- 0.05           # 5%
SPINNER_LOW_THRESHOLD <- 0.20   # 20%
SPINNER_HIGH_THRESHOLD<- 0.40   # 40%

# HELPER VALUES
AVERAGE_RANGE <- mean(df$range)
AVERAGE_BODY_SIZE <- mean(df$body)

average_body_fifths = c()
for(i in seq(from=1, to=5)) {
  average_body_fifths <- c(average_body_fifths, AVERAGE_BODY_SIZE*0.2*(i-1))
}
print(average_body_fifths)

# CANDLESTICK TYPES
DOJI_FOUR_STAR          = "FOUR STAR DOJI"
DOJI_COMMON             = "COMMON DOJI"
DOJI_BULL_DRAGONFLY     = "BULL DRAGONFLY DOJI"
DOJI_BEAR_DRAGONFLY     = "BEAR DRAGONFLY DOJI"
DOJI_CROSS              = "CROSS DOJI"
DOJI_INVERTED_CROSS     = "INVERTED CROSS DOJI"
DOJI_BULL_GRAVESTONE    = "BULL GRAVESTONE DOJI"
DOJI_BEAR_GRAVESTONE    = "BEAR GRAVESTONE DOJI"
MARUBOZU_BULL_OPENING   = "BULL OPENING MARUBOZU"
MARUBOZU_BULL_CLOSING   = "BULL CLOSING MARUBOZU"
MARUBOZU_BULL_IDEAL     = "IDEAL BULL MARUBOZU"
MARUBOZU_BEAR_OPENING   = "BEAR OPENING MARUBOZU"
MARUBOZU_BEAR_CLOSING   = "BEAR CLOSING MARUBOZU"
MARUBOZU_BEAR_IDEAL     = "IDEAL BEAR MARUBOZU"
SPINNER_COMMON          = "COMMON SPINNER"
SPINNING_TOP            = "SPINNING TOP"
SPINNING_BOTTOM         = "SPINNING BOTTOM"

# Determine classification for each candlestick
for(i in seq(from=1, to=nrow(df))){
  candlestick <- NaN
  current <- df[i,]
  
  # Partition the current candlestick price range in levels of fifths
  #   Low | Low + 20% | Low + 40% | Low + 60% | Low + 80%
  current_fifths = c()
  for(i in seq(from=1, to=5)){
    current_fifths = c(current_fifths, current$Low + (current$range*0.2*(i-1)))
  }

  ####################################
  #           DOJI CANDLES           #
  ####################################
  dojiError <- DOJI_ERROR * current$range

  # FOUR STAR DOJI
  #
  # The Open price of the day equals the Close price of the day
  
  if(current$Open == current$Close){
    candlestick <- DOJI_FOUR_STAR
  }
  
  # COMMON DOJI
  #
  # The average body size is within the Doji Error (+/-7%) of the
  # midpoint of the price range. Also, the body of the candle should
  # be smaller than 40% of the overall average body size within the 
  # time period under consideration.
  if( (current$body_average + dojiError > current$mid_point) &&
      (current$body_average - dojiError < current$mid_point) &&
      (current$body < average_body_fifths[2]) ){
    candlestick <- DOJI_COMMON
  }
  
  # CROSS DOJI
  #
  # A Cross Doji is a Doji where the body average is above 80% of the price
  # range. 
  if( (current$body_average > current_fifths[4]) &&
      (current$body < average_body_fifths[2]) ){
    candlestick <- DOJI_CROSS
  }
  
  # BULL DRAGONFLY DOJI
  #
  # Dragonfly Dojis are Dojis where the body is at the very top of the 
  # candlestick.
  #
  # Here, a Bull Dragonfly Doji is one where the Close price equals the 
  # High price of the day.
  if( (current$body_average > current_fifths[4]) &&
      (current$body < average_body_fifths[2]) &&
      (current$direction == "UP") &&
      (current$High == current$Close)
  ) {
    candlestick <- DOJI_BULL_DRAGONFLY
  }
  
  # BEAR DRAGONFLY DOJI
  #
  # Here, a Bear Dragonfly Doji is one where the Open price equals the 
  # High price of the day.
  if( (current$body_average > current_fifths[4]) &&
      (current$body < average_body_fifths[2]) &&
      (current$direction == "DOWN") &&
      (current$High == current$Open)
  ) {
    candlestick <- DOJI_BEAR_DRAGONFLY
  }
  
  # INVERTED CROSS DOJI
  #
  # An Inverted Cross Doji is a Doji where the body average is below 20% of
  # the price range.
  if ( (current$body_average < current_fifths[2]) &&
       (current$body < average_body_fifths[2]) ){
    candlestick <- DOJI_INVERTED_CROSS
  }
  
  # BULL GRAVESTONE DOJI
  #
  # Here, a Bull Gravestone Doji is a Doji where the open price is equal to the low price.
  if( (current$body_average < current_fifths[2]) &&
      (current$body < average_body_fifths[2]) &&
      (current$direction == "UP") &&
      (current$Low == current$Open)
  ) {
    candlestick <- DOJI_BULL_GRAVESTONE
  }
  
  # BEAR GRAVESTONE DOJI
  #
  # Here, a Bear Gravestone Doji is a Doji where the close price is equal to the low price.
  if( (current$body_average < current_fifths[2]) &&
      (current$body < average_body_fifths[2]) &&
      (current$direction == "DOWN") &&
      (current$Low == current$Close)
  ) {
    candlestick <- DOJI_BEAR_GRAVESTONE
  }
  
  ####################################
  #         MARUBOZU CANDLES         #
  ####################################
  marubozuError <- MARUBOZU_ERROR * current$range
  
  # BULL OPENING MARUBOZU
  #
  # Marubozu candles are defined by the body taking up 80% or more of the 
  # candle's price range.
  #
  # A Bull Opening Marubozu is one where the opening price is very close to 
  # the Low of the day.
  if( (current$body > (current$range * MARUBOZU_THRESHOLD)) &&
      (current$direction == "UP") &&
      ((current$Low + marubozuError) > current$Open) ){
    candlestick <- MARUBOZU_BULL_OPENING
  }
  
  # BULL CLOSING MARUBOZU
  #
  # A Bull Closing Marubozu is one where the closing price is very close to the
  # High of the day.
  if( (current$body > (current$range * MARUBOZU_THRESHOLD)) &&
      (current$direction == "UP") &&
      ((current$High - marubozuError) < current$Close) ){
    candlestick <- MARUBOZU_BULL_CLOSING
  }
  
  # IDEAL BULL MARUBOZU
  #
  # An ideal Bull Marubozu is one where the closing price is very close to the 
  # high and the opening price is very close to the low.
  if( (current$body > (current$range * MARUBOZU_THRESHOLD)) &&
      (current$direction == "UP") &&
      ((current$High - marubozuError) < current$Close) &&
      ((current$Low + marubozuError) > current$Open) ){
    candlestick <- MARUBOZU_BULL_IDEAL
  }
  
  # BEAR OPENING MARUBOZU
  #
  # Marubozu candles are defined by the body taking up 80% or more of the 
  # candle's price range.
  #
  # A Bear Opening Marubozu is one where the opening price is very close to 
  # the High of the day.
  if( (current$body > (current$range * MARUBOZU_THRESHOLD)) &&
      (current$direction == "DOWN") &&
      ((current$High - marubozuError) < current$Open) ){
    candlestick <- MARUBOZU_BEAR_OPENING
  }
  
  # BEAR CLOSING MARUBOZU
  #
  # A Bear Closing Marubozu is one where the closing price is very close to 
  # the Low of the day.
  if( (current$body > (current$range * MARUBOZU_THRESHOLD)) &&
      (current$direction == "DOWN") &&
      ((current$Low + marubozuError) < current$Close) ){
    candlestick <- MARUBOZU_BEAR_CLOSING
  }
  
  # IDEAL BEAR MARUBOZU
  #
  # An ideal Bear Marubozu is one where the closing price is very close to the 
  # low and the opening price is very close to the high.
  if( (current$body > (current$range * MARUBOZU_THRESHOLD)) &&
      (current$direction == "DOWN") &&
      ((current$High - marubozuError) < current$Open) &&
      ((current$Low + marubozuError) > current$Close) ){
    candlestick <- MARUBOZU_BEAR_IDEAL
  }
  
  ####################################
  #          SPINNER CANDLES         #
  ####################################
  spinnerError <- SPINNER_ERROR * current$range
  
  
  # COMMON SPINNER
  #
  # A Spinner is a candle where the body size is between 20% and 40% of the 
  # total price range. A Common spinner is one where the body is centered about
  # price range.
  if( (current$body > (current$range * SPINNER_LOW_THRESHOLD)) &&
      (current$body < (current$range * SPINNER_HIGH_THRESHOLD)) &&
      ((current$body_average + spinnerError) > current$mid_point) &&
      ((current$body_average - spinnerError) < current$mid_point) ){
    candlestick <- SPINNER_COMMON
  }
  
  # SPINNING TOP
  #
  # A Spinning Top is a type of spinner where the body average is centered at 
  # 60% or more of the price range.
  if ( (current$body > (current$range * SPINNER_LOW_THRESHOLD)) &&
       (current$body < (current$range * SPINNER_HIGH_THRESHOLD)) &&
       (current$body_average > current_fifths[4]) ){
    candlestick <- SPINNING_TOP
  }
  
  # SPINNING BOTTOM
  #
  # A Spinning Bottom is a type of spinner where the body average is centered 
  # at 40% or less of the price range.
  if ( (current$body > (current$range * SPINNER_LOW_THRESHOLD)) &&
       (current$body < (current$range * SPINNER_HIGH_THRESHOLD)) &&
       (current$body_average < current_fifths[2]) ){
    candlestick <- SPINNING_BOTTOM
  }
  
  print(paste(current$Date, candlestick, sep=" "))

}
