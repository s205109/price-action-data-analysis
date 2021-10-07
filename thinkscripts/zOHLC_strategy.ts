declare upper;
declare once_per_bar;

input start_of_day = 930;		# 9:30 AM
input end_of_day = 1600;		# 4:00 PM

def adjStart = start_of_day;
def adjEnd = end_of_day;

def agg_period = GetAggregationPeriod();

def buy_trigger = if agg_period >= AggregationPeriod.DAY then 1 else if SecondsTillTime(adjEnd) >= 60 and SecondsFromTime(adjStart) >= -60 then 1 else 0;

AddOrder(OrderType.BUY_TO_OPEN,
	buy_trigger,
	low,
	1,
	Color.Black,
	Color.Black, name="SOHLCP|"+GetSymbol()+"|"+open[-1]+"|"+high[-1]+"|"+low[-1]+"|"+close[-1]+"|"+close);

AddOrder(OrderType.SELL_TO_CLOSE, buy_trigger, high, 1, Color.Black, Color.Black, name="SellClose");