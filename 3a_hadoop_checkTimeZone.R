library(rmr2)
rmr.options(backend="local")

### READ TICKS
map.ticks.withoutdate <- function(k, v) {
  # Skip header lines
  v = v[v$PRICE != "PRICE",]
  
  time = substr(v$TIMESTAMP, 12, 13)
  # time = v$TIMESTAMP
  
  keyval(key=time,
         val=1)
}

red.ticks <- function(k, v) {
  keyval(key=k, 
         val=sum(v))
}

col.names   = c("WKN", "ISIN", "INSTRUMENT_NAME", "TIMESTAMP", "HSEC", "PRICE", "UNITS", "BID_ASK_FLAG")
inputformat <- make.input.format("csv", sep = ";",
                                 col.names=col.names,
                                 stringsAsFactors = FALSE) 
important.cols = c("TIMESTAMP")

data <- mapreduce(input="data/1017_01_M_08_E_20090331/monthly_bba_aa_20090331.csv",
                  input.format=inputformat,
                  map = map.ticks.withoutdate
                  ,reduce = red.ticks
                 )

data.df <- from.dfs(data)
# View(data.df)

d = data.frame(key=rep(data.df$key,sapply(data.df$key, length)), value=unlist(data.df$val))
d$key = as.character(d$key)

for (h in 0:23) {
  if (!sprintf("%02d", h) %in% d$key) {
    row = c(sprintf("%02d", h), as.numeric(0))
    d = rbind(d, c(sprintf("%02d", h), as.numeric(0)))
  }
}
d$value = as.numeric(d$value)
d = d[order(d$key),]

min = substring(min(x[!is.na(x)]), 12, 19)
max = substring(max(x[!is.na(x)]), 12, 19)
pdf("plots/trade_distribution_new.pdf", height=4, width=10)
barplot(d$val / sum(d$val), names.arg = d$key, main="Trades distribution over daytime",
        sub = paste("earliest trade:", min, "- latest trade:", max, "- total of", format(sum(y), big.mark=","), "trades"), ylab="density", xlab="daytime hours")
dev.off()
