library(rmr2)
rmr.options(backend="local")
#  Seconds include HSEC
options(digits.secs=2)

### READ TICKS
map.ticks <- function(k, v) {
  time = substr(v$TIMESTAMP, 12, 19)
  
  keyval(key=time,
         val=1)
}
#debug(map.ticks)
red.ticks <- function(k, v) {
  
  
  keyval(key=k, 
         val=sum(v))
}

#debug(red.ticks)
col.names   = c("WKN", "ISIN", "INSTRUMENT_NAME", "TIMESTAMP", "HSEC", "PRICE", "UNITS", "BID_ASK_FLAG")
col.classes = c(rep("character",4), "integer", "double", "integer", "character")
inputformat <- make.input.format("csv", sep = ";",
                                 col.names=col.names,
                                 colClasses=col.classes) 
important.cols = c("TIMESTAMP")

data <- mapreduce(input="data/1017_01_M_08_E_20090331/monthly_bba_aa_20090331.csv",
                  input.format=inputformat,
                  map = map.ticks
                  ,reduce = red.ticks
                 )

data.df <- from.dfs(data)
View(data.df)

x = as.POSIXct(data.df$key, format = "%H:%M:%OS")
y = data.df$val

min = substring(min(x[!is.na(x)]), 12, 19)
max = substring(max(x[!is.na(x)]), 12, 19)

jpeg("plots/trade_distribution.jpeg")
plot(x, y, main = "Trades distribution over daytime", ylab="number of trades in whole dataset", xlab="daytime",
       sub = paste("first trade:", min, "- last trade:", max, "- total of", sum(y), "trades"))
dev.off()
hist(x, breaks="min", freq=TRUE)
