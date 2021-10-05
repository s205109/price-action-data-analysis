declare upper;
declare once_per_bar;

input startTime = 930;
input endTime = 1600;

def adjStartTime = startTime;
def adjEndTime = endTime;
def agg = GetAggregationPeriod();

def marketOpen = if agg >= AggregationPeriod.DAY then 1 else if SecondsTillTime(adjEndTime) >= 60 and SecondsFromTime(adjStartTime) >= -60 then 1 else 0;

def macdValue = MACD().Value;
def macdAvg = MACD().Avg;

AddOrder(OrderType.BUY_TO_OPEN,
    marketOpen,
    low,
    1,
    Color.White,
    Color.White, name="SOHLCP|"+GetSymbol()+"|"+open[-1]+"|"+high[-1]+"|"+low[-1]+"|"+close[-1]+"|"+close);

AddOrder(OrderType.SELL_TO_CLOSE, marketOpen, high, 1, Color.White, Color.White, name="SellClose");
