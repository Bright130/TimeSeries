library(readr)
stock<- read_csv("MacroTrends_WFM.csv",  col_types = cols(date = col_date(format = "%d-%m-%y")))
plot(stock$date,stock$close)
lag.plot(stock$close)
acf(stock$close,60,main="Whole Foods Market daily closing stock price")

#for stationary look at ln.r 