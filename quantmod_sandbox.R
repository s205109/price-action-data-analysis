install.packages("quantmod")
library(quantmod)

Amazon<-getSymbols(
  "AMZN", 
  from = "2015-01-01", 
  to = "2021-03-17", 
  periodicity = "daily", 
  auto.assign=FALSE
)

head(Amazon$AMZN.Open)
head(Amazon$AMZN.Close)