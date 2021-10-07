declare upper;
declare once_per_bar;


def isBefore = HasEarnings(EarningTime.BEFORE_MARKET);
def isAfter = HasEarnings(EarningTime.AFTER_MARKET);
def actualEarnings = if !IsNaN(GetActualEarnings()) then GetActualEarnings() else -1;
def estimatedEarnings = if !IsNaN(GetEstimatedEarnings()) then GetEstimatedEarnings() else -1;


AddOrder(OrderType.BUY_TO_OPEN,
    HasEarnings(),
    low,
    1,
    Color.Black,
    Color.Black, name=""+GetSymbol()+"|"+isBefore+"|"+isAfter+"|"+actualEarnings+"|"+estimatedEarnings);

AddOrder(OrderType.SELL_TO_CLOSE, HasEarnings(), high, 1, Color.Black, Color.Black, name="SellClose");
