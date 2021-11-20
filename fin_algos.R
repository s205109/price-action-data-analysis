
# Max Drawdown Function
mdd <- function(rets, ex_stat=NaN) {
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



variance <- function(col) {
  sqrt(sd(col))
}
