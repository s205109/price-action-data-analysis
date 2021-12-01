#setwd("/Users/dominicsciarrino/Documents/projects/trading/price-action-data-analysis/phase6_shiny-app")

library(shiny)
library(stringr)
library(dplyr)
library(plotly)
library(ggplot2)
library(lubridate)

#
# get_stock_list
#
# Returns a list of the available stocsk
#
get_stock_list <- function(){
    data_processed_path <- paste(getwd(), "/data", sep="")
    
    csv_files <- list.files(
        path = data_processed_path
    )
    
    stcks = c()
    for(file in csv_files){
        stcks <- c(stcks, gsub(pattern = "\\.csv$", "", basename(file)))
    }
    stcks <- sort(stcks)
    stcks
}

#
# get_stock_prices
#
# Returns the historical prices of the stock 'ticker'
#
get_stock_prices <- function(ticker) {
    # Obtain stock pricing
    data_processed_path <- paste(getwd(), "/data", sep="")
    csv_filename <- paste0(data_processed_path, "/", ticker, ".csv")
    df_pricing <- read.csv(csv_filename)
    df_pricing <- df_pricing %>% select('Date','Close', 'Open')
    df_pricing$Date <- as.Date(df_pricing$Date, format="%m-%d-%y")
    
    df_pricing
}

#
# get_benchmark_plot
#
# Returns a basic pricing plot
#
get_benchmark_plot <- function(ticker){
    df_prices <- get_stock_prices(ticker)
    g <- ggplot(df_prices, aes(x=Date, y=Close)) + geom_line()
    g
}

#
# get_ohlc_prices
#
# Returns the historical OHLC prices of the stock 'ticker'
#
get_ohlc_prices <- function(ticker) {
    # Obtain stock pricing
    data_processed_path <- paste(getwd(), "/data", sep="")
    csv_filename <- paste0(data_processed_path, "/", ticker, ".csv")
    ohlc_pricing <- read.csv(csv_filename)
    ohlc_pricing <- ohlc_pricing %>% select('Date', 'Open', 'High', 'Low', 'Close')
    ohlc_pricing$Date <- as.Date(ohlc_pricing$Date, format="%m-%d-%y")
    
    ohlc_pricing
}

#
# get_post_earnings_range
#
# Get ranges ranges determined by POST_EARNINGS_INTERVAL.
#
get_post_earnings_range <- function(ticker, post_earnings_interval){
    # Obtain list of earnings dates from stock ticker
    earnings_dates <- get_earnings_dates(ticker)
    
    # Get trading dates for stock ticker
    data_processed_path <- paste(getwd(), "/data", sep="")
    csv_filename <- paste0(data_processed_path, "/", ticker, ".csv")
    df_pricing <- read.csv(csv_filename)
    trading_dates <- read.csv(csv_filename) %>% select('Date')
    
    e_date_ctr <- 1
    earnings_dates_ranges <- data.frame(
        startDate=character(),
        endDate=character()
    )
    for(i in seq(from=1, to=nrow(trading_dates))){
        if(e_date_ctr > nrow(earnings_dates)){
            break
        }
        # Detected an earnings date
        if(trading_dates[i,"Date"] == earnings_dates[e_date_ctr, "Date"]){
            # Calculate post earnings date range
            post_earnings_start_date <- trading_dates[i+1,"Date"]
            post_earnings_end_date <- trading_dates[i+post_earnings_interval, "Date"]
            post_earnings_range <- c(post_earnings_start_date, post_earnings_end_date)
            
            earnings_dates_ranges[nrow(earnings_dates_ranges)+1,] <- post_earnings_range
            
            e_date_ctr <- e_date_ctr + 1
        }
    }
    earnings_dates_ranges
}


#
# get_earnings_info
#
# Returns the list, in chronological order, of earnings date for 
# a particular stock
#
get_earnings_info <- function(stock, post_earnings_interval=15){
    data_processed_path <- paste(getwd(),"/data_earnings", sep="")
    nasdaq_earnings <- read.csv(paste(data_processed_path, "NASDAQ100_Earnings.csv", sep="/"))
    dow30_earnings <- read.csv(paste(data_processed_path, "DOW30_Earnings.csv", sep="/"))
    earnings <- merge(dow30_earnings, nasdaq_earnings, all=TRUE)
    
    earnings <- earnings %>% filter(Ticker == stock)
    earnings$Date <- as.Date(earnings$Date, format="%m-%d-%y")
    earnings <- earnings %>% arrange(Date)
    
    post_earnings_ranges <- get_post_earnings_range(stock, post_earnings_interval)
    if(nrow(earnings) > nrow(post_earnings_ranges)){
        earnings <- earnings[-nrow(earnings),]
    }
    
    earnings$startDate <- post_earnings_ranges$startDate
    earnings$endDate <- post_earnings_ranges$endDate
    
    earnings
}

#
# get_earnings_dates
#
# Returns the dates where stock had earnings
#
get_earnings_dates <- function(stock){
    data_processed_path <- paste(getwd(),"/data_earnings", sep="")
    nasdaq_earnings <- read.csv(paste(data_processed_path, "NASDAQ100_Earnings.csv", sep="/"))
    dow30_earnings <- read.csv(paste(data_processed_path, "DOW30_Earnings.csv", sep="/"))
    earnings <- merge(dow30_earnings, nasdaq_earnings, all=TRUE)
    
    earnings <- earnings %>% filter(Ticker == stock)
    earnings_dates <- earnings %>% select('Date') %>% arrange(as.Date(earnings$Date, format="%m-%d-%y"))
    
    earnings_dates
}

#
# get_gap_plot
#
# Get the stock price plot
#
get_gap_plot <- function(ticker, stop_loss_factor){
    # Obtain stock pricing
    df_pricing <- get_stock_prices(ticker)
    
    gap_order_dates <- determine_gap_orders(ticker, stop_loss_factor)
    
    g <- ggplot(df_pricing, aes(x=Date, y=Close)) + geom_line()
    
    colors <- c()
    for(i in seq(from=1, to=nrow(gap_order_dates))){
        if (gap_order_dates[i,]$BuyDate == gap_order_dates[i,]$SellDate){
            g <- g + 
                geom_vline(
                    xintercept = gap_order_dates[i,]$SellDate, 
                    color="black"
                )
            
        }
        else{
            g <- g +
                geom_vline(
                    xintercept = gap_order_dates[i,]$BuyDate,
                    color="green"
                )
            
            g <- g +
                geom_vline(
                    xintercept = gap_order_dates[i,]$SellDate,
                    color="red"
                )
            
        }
    }
    
    g
}


#
# get_stock_plot
#
# Returns a plot of the input stock ticker
#
get_stock_plot <- function(ticker, post_earnings_interval=15){
    # Obtain stock pricing
    df_pricing <- get_stock_prices(ticker)
    
    # Obtain earnings information
    earnings <- get_earnings_info(ticker, post_earnings_interval)
    earnings$startDate <- as.Date(earnings$startDate, format="%m-%d-%y")
    earnings$endDate <- as.Date(earnings$endDate, format="%m-%d-%y")
    
    g <- ggplot(df_pricing, aes(x=Date, y=Close)) + geom_line()
    
    colors <- c()
    for(i in seq(from=1, to=nrow(earnings))){
        # Determine color of earnings period, based on expected vs.
        #    actual returns
        if(earnings[i,]$AEPS > earnings[i,]$EEPS){
            col <- "green"
        }
        else if(earnings[i,]$AEPS < earnings[i,]$EEPS){
            col <- "red"
        }
        else{
            col <- "black"
        }
        colors <- c(colors, col)
        
        g <- g + geom_vline(xintercept = as.Date(earnings[i,]$Date, format="%m-%d-%y"), color=col)
    }
    
    g <- g +
        geom_rect(
            data=earnings,
            inherit.aes = FALSE,
            aes(
                xmin=startDate,
                xmax=endDate,
                ymin=min(df_pricing$Close),
                ymax=max(df_pricing$Close)
            ),
            color="transparent",
            fill=colors,
            alpha=0.3
        )

    g
}

#
# earnings_trade_criteria
#
# Computes the criteria for earnings zone trade
#
earnings_trade_criteria <- function(df_earnings, df_pricing, post_earnings_interval){
    post_dates <- c()
    trade_status <- c()
    earnings_announced <- c()
    e <- 1
    buy_back_date <- NULL
    
    for(i in seq(from=1, to=nrow(df_pricing))){
        # Determine the post-earnings zone dates
        if( (i + post_earnings_interval) >= nrow(df_pricing) ){
            post_date <- 0
        }
        else{
            post_date <- df_pricing[i+post_earnings_interval, 1]
        }
        
        # Determine earnings dates
        current_date <- df_pricing[i,]$Date
        announced <- FALSE
        if( nrow(df_earnings) > e ){
            if( current_date == df_earnings$Date[e] ){
                announced <- TRUE
                e <- e + 1
            }
        }
        
        # Determine trade execution status
        if(i == 1){
            trade_executed <- TRUE
        }
        else if(i > 1){
            # Days where earnings were announced
            if (trade_executed && announced) {
                trade_executed <- FALSE
                buy_back_date <- post_date
            }
            # Days between the sell date and the post-earnings zone date
            else if(!trade_executed && !announced){
                if (buy_back_date == current_date){
                    trade_executed <- TRUE
                    buy_back_date <- NULL
                    buy_price <- df_pricing[i,]$Open
                }
            }
        }
        trade_status <- c(trade_status, trade_executed)
        earnings_announced <- c(earnings_announced, announced)
        post_dates <- c(post_dates, post_date)
    }
    post_dates <- as.Date(post_dates, origin="1970-01-01")
    
    criteria <- data.frame(
        EarningsDate = earnings_announced,
        PostDate = post_dates,
        TradeStatus = trade_status)
    
    criteria
}

#
# trade_earnings
#
# Carries out earnings zone trade. Computes returns.
#
trade_earnings <- function(ticker, post_earnings_interval){
    
    earnings_info <- get_earnings_info(ticker, post_earnings_interval)
    pricing <- get_stock_prices(ticker)
    
    trade_criteria <- earnings_trade_criteria(earnings_info, pricing, post_earnings_interval)
    
    investment <- 1
    account <- c()
    returns <- c()
    
    for(j in seq(from=1, to=nrow(pricing))){
        trade_status <- trade_criteria[j,]$TradeStatus
        isEarnings <- trade_criteria[j,]$EarningsDate
        postDate <- trade_criteria[j,]$PostDate
        
        # Holding the stock
        if(!isEarnings && trade_status){
            if(j==1){
                buyPrice <- pricing[j,]$Open
            }
            else if(!trade_criteria[j-1,]$TradeStatus){
                buyPrice <- pricing[j,]$Close
            }
        }
        
        # Earnings Day
        if(isEarnings){
            sellPrice <- pricing[j-1,]$Close
        }
        else if(j==nrow(pricing)){
            sellPrice <- pricing[j,]$Close
        }
        
        # Actual Returns
        if(isEarnings || j==nrow(pricing)){
            return <- 1 + ((sellPrice - buyPrice) / buyPrice)
        }
        else{
            return <- 1
        }
        
        if(j==1){
            investment <- 1
        }
        else{
            investment <- return*investment
        }
        
        
        returns <- c(returns, return)
        account <- c(account, investment)
    }
    
    df_returns <- data.frame(
        Date=pricing$Date,
        Account=account
    )
    
    df_returns <- df_returns[-nrow(df_returns),]
    
    df_returns
}

#
# get_gap_returns_plot
#
# Get the returns plot for gap strategy
#
get_gap_returns_plot <- function(ticker, stop_loss_factor){
    gap_df <- determine_gap_orders(ticker, stop_loss_factor)
    
    investment <- 1
    idx <- 1
    idxs <- c()
    returns <- c()
    for(i in seq(from=1, to=nrow(gap_df))){
        sellPrice <- gap_df[i,]$SellAmount
        buyPrice <- gap_df[i,]$BuyAmount
        
        r <- 1 + ((sellPrice - buyPrice) / buyPrice)
        investment <- r*investment
        returns <- c(returns, investment)
        idxs <- c(idxs, idx)
        idx <- idx + 1
    }
    
    plot_df <- data.frame(
        TradeID=idxs,
        Return=returns
    )
    
    g <- ggplot(plot_df, aes(x=TradeID, y=Return)) + geom_line()
    g
}

#
# get_returns_plot()
#
# Returns a plot of the Account column, given in this method 
#
get_returns_plot <- function(df_returns){
    g <- ggplot(df_returns, aes(x=Date, y=Account)) + geom_line()
    g
}

#
# determine_gap_orders
#
# Determines the buy and sell orders for gap strategy.
#
determine_gap_orders <- function(ticker, stop_loss_factor = 0.5){
    # Get pricing
    prices <- get_ohlc_prices(ticker)
    
    trade_currently_executed <- FALSE
    
    current_trade_buy_price <- 0
    current_trade_sell_target <- 0
    current_trade_stop_target <- 0
    
    buy_orders <- c()
    buy_order_dates <- c()
    buy_order_amount <- c()
    sell_orders <- c()
    sell_order_dates <- c()
    sell_order_amount <- c()

    for(i in seq(from=1, to=nrow(prices))){
        #################################
        #      Detect Buy Signals       #
        #################################
        
        # Detect Downward Gap
        if (i != 1 && prices[i-1,]$Low > prices[i,]$Open && trade_currently_executed == FALSE){
            # Create a buy order for the open of day i
            buy_orders <- c(buy_orders, prices[i,]$Open)
            buy_order_amount <- c(buy_order_amount, prices[i,]$Open)
            buy_order_dates <- c(buy_order_dates, prices[i,]$Date)
            
            current_trade_buy_price <- prices[i,]$Open
            
            # Create a sell target for the future
            current_trade_sell_target <- prices[i-1,]$Low
            current_trade_stop_target <- current_trade_buy_price * (1-stop_loss_factor)
            
            # Program flag to make sure only one trade is open at a time.
            trade_currently_executed <- TRUE
        }
        # Downward Gap NOT detected
        else {
            # Zero out the buy order
            buy_orders <- c(buy_orders, 0)
            
            # Trade currently happening? No -> Zero it out
            if(trade_currently_executed == FALSE){
                current_trade_sell_target <- 0
                current_trade_stop_target <- 0
            }
        }
        
        #################################
        #      Detect Sell Signals      #
        #################################
        
        # Meeting the Stop Target
        if(trade_currently_executed && prices[i,]$Low <= current_trade_stop_target){
            sell_orders <- c(sell_orders, current_trade_stop_target)
            sell_order_dates <- c(sell_order_dates, prices[i,]$Date)
            sell_order_amount <- c(sell_order_amount, current_trade_stop_target)
            r <- 1+((current_trade_stop_target-current_trade_buy_price)/current_trade_buy_price)
            trade_currently_executed <- FALSE
            current_trade_buy_price <- 0
            current_trade_sell_target <- 0
            current_trade_stop_target <- 0
        }
        
        # Meeting the Sell Target
        else if(trade_currently_executed && prices[i,]$High > current_trade_sell_target){
            sell_orders <- c(sell_orders, current_trade_sell_target)
            sell_order_dates <- c(sell_order_dates, prices[i,]$Date)
            sell_order_amount <- c(sell_order_amount, current_trade_sell_target)
            r <- 1+((current_trade_sell_target-current_trade_buy_price)/current_trade_buy_price)
            trade_currently_executed <- FALSE
            current_trade_buy_price <- 0
            current_trade_sell_target <- 0
            current_trade_stop_target <- 0
        }
        
        # Neither of the sell signals are met
        else{
            sell_orders <- c(sell_orders, 0)
            r<-1
        }
    }
    
    if(length(buy_order_dates) > length(sell_order_dates) ){
        buy_order_dates <- buy_order_dates[-length(buy_order_dates)]
        buy_order_amount <- buy_order_amount[-length(buy_order_amount)]
    }
    
    order_dates <- data.frame(
        BuyDate=buy_order_dates,
        BuyAmount=buy_order_amount,
        SellDate=sell_order_dates,
        SellAmount=sell_order_amount
    )
    
    order_dates
}

#
# benchmark_return
#
# Returns the current return for buy and hold.
#
benchmark_return <- function(ticker){
    df_prices <- get_stock_prices(ticker)
    
    buyPrice <- df_prices[1,]$Open
    sellPrice <- df_prices[nrow(df_prices),]$Close
    
    return <- (1 + (sellPrice - buyPrice) / buyPrice) * 100
    
    retVal <- paste("Total Return: ", return, "%")
    retVal
}

#
#
#
#
#
maxDD_benchmark <- function(ticker){
    df_prices <- get_stock_prices(ticker)
    closing_prices <- df_prices$Close
    
    returns <- c()
    for(i in seq(from=2, to=length(closing_prices))){
        r <- 1 + ((closing_prices[i] - closing_prices[i-1])/closing_prices[i-1])
        returns <- c(returns, r)
    }
    
    max_dd <- mdd(returns)
    
    retVal <- paste("Maximum Drawdown (%): ", max_dd)
    retVal
}

mdd <- function(rets) {
    r <- 1
    investment <- 1
    peak <- -100000
    maxDD <- 0
    
    mdd_df <- data.frame(Iteration=integer(),
                         Return=double(),
                         Investment=double(),
                         Peak=double(),
                         Drawdown=double(),
                         Drawdown_Percent=double())
    
    for(i in seq(from=1, to=length(rets))){
        # Find Returns
        r <- as.double(rets[i])
        
        # Calculate the current investment amount
        investment <- investment * rets[i]
        
        # Update peak if necessary
        if (investment > peak) {
            peak <- investment
        }
        
        # Calculate the drawdown
        dd <- peak - investment
        
        # Calculate the drawdown percentage
        dd_percent <- (dd / peak)*100
        
        mdd_df[nrow(mdd_df)+1,] <- c(i, r, investment, peak, dd, dd_percent)
    }
    
    # Return the drawdown as a percentage and a negative value
    -1*max(mdd_df$Drawdown_Percent)
}







# Tab 1: Earnings Zone Backtest
page_one <- tabPanel(
    "Backtest 1",
    titlePanel("Earnings Zone Backtest"),
    sidebarLayout(
        sidebarPanel(
            selectInput('stock_select', "Stock:", "AAPL"),
            numericInput('post_earnings_interval',
                         label = "Post-Earnings Interval",
                         value=15,
                         min=1,
                         max=50)
        ),
        mainPanel(
            h3("5-Yr Historical Prices"),
            textOutput(outputId='current_ticker'),
            plotOutput(outputId = "plot_prices"),
            plotOutput(outputId = 'return_plot')
        )
    )
)

page_two <- tabPanel(
    "Backtest 2",
    titlePanel("Gap Down Backtest"),
    sidebarLayout(
        sidebarPanel(
            selectInput('stock_select2', "Stock:", "AAPL"),
            numericInput('stop_loss_factor',
                         label = "Stop Loss Factor",
                         value=0.5,
                         min=0.01,
                         max=1,
                         step=0.01)
        ),
        mainPanel(
            h3("5-Yr Historical Prices"),
            textOutput(outputId='current_ticker2'),
            plotOutput(outputId='plot_prices_gap'),
            plotOutput(outputId='plot_gap_strategy_returns')
        )
    )
)

page_zero <- tabPanel(
    "Benchmark",
    titlePanel("Benchmark"),
    sidebarLayout(
        sidebarPanel(
            selectInput('stock_select3', "Stock:", "AAPL"),
            textOutput(outputId="benchmark_return"),
            textOutput(outputId="max_drawdown_benchmark")
        ),
        mainPanel(
            h3("5-Yr Historical Prices"),
            textOutput(outputId='current_ticker3'),
            plotOutput(outputId='plot_benchmark_prices')
        )
    )
)


# UI
ui <- navbarPage(
    "Stock Backtester",
    page_zero,
    page_one,
    page_two
)







# Server
server <- function(input, output, session) {
    # Stock Selector
    stocks <- get_stock_list()
    updateSelectInput(session, "stock_select", choices=stocks)
    updateSelectInput(session, "stock_select2", choices=stocks)
    updateSelectInput(session, "stock_select3", choices=stocks)
    
    
    # Historical Prices Plot
    output$plot_prices <- renderPlot({
        get_stock_plot(input$stock_select, input$post_earnings_interval)
    })
    # Current Ticker
    output$current_ticker <- renderText({
        input$stock_select
    })
    # Current Ticker 2
    output$current_ticker2 <- renderText({
        input$stock_select2
    })
    # Current Ticker 3
    output$current_ticker3 <- renderText({
        input$stock_select3
    })
    
    # Earnings Date
    output$earnings_dates <- renderTable({
        get_earnings_dates(input$stock_select)
    })
    
    # Render returns plot
    output$return_plot <- renderPlot({
        get_returns_plot(
            trade_earnings(input$stock_select, input$post_earnings_interval)
        )
    })
    
    # Render gap strategy plot
    output$plot_prices_gap <- renderPlot({
        get_gap_plot(input$stock_select2, input$stop_loss_factor)
    })
    
    # Render gap strategy returns plot
    output$plot_gap_strategy_returns <- renderPlot({
        get_gap_returns_plot(input$stock_select2, input$stop_loss_factor)
    })
    
    # Render benchmark stock price plot
    output$plot_benchmark_prices <- renderPlot({
        get_benchmark_plot(input$stock_select3)
    })
    
    # Get benchmark returns
    output$benchmark_return <- renderText({
        benchmark_return(input$stock_select3)
    })
    
    output$max_drawdown_benchmark <- renderText({
        maxDD_benchmark(input$stock_select3)
    })
}



# Run the app
shinyApp(ui = ui, server = server)
