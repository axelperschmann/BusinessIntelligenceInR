library(rmr2)
rmr.options(backend="local")
#  Seconds include HSEC
options(digits.secs=2)

# load isin_set
ev = read.csv("events.csv", sep = ",", as.is=TRUE, col.names=c("date", "ISIN"))
isin.list = as.character(unique(ev[,2]))

### READ TICKS
col.names=c("WKN", "ISIN", "INSTRUMENT_NAME", "TIMESTAMP", "HSEC", "PRICE", "UNITS", "BID_ASK_FLAG")
inputformat <- make.input.format("csv", sep = ";", col.names=col.names, colClasses=c(rep("character",4), "integer", "double", "integer", "character")) 
important.cols = c("ISIN", "TIMESTAMP", "PRICE", "BID_ASK_FLAG", "useful", "count")

map.ticks <- function(k, v) {
  v = v[v$BID_ASK_FLAG == "A",]
  v = v[v$PRICE != 0.0, ]
  
  # ts = paste(v[["TIMESTAMP"]], ".", v[["HSEC"]], sep="")
  value = cbind(v, useful=v$ISIN %in% isin.list, count=1)
  
  # only extract ISIN's when we have observed an AdHoc msg.
  value = value[value$useful == TRUE,]
  
  keyval(key=value$ISIN,
         val=subset(value, select=important.cols))
}

red.ticks <- function(k, v) {
    v$TIMESTAMP = as.POSIXct(v$TIMESTAMP, "%Y-%m-%d %H:%M:%OS", tz="CET")
    v = v[order(v$TIMESTAMP),]
    
    adhocs = ev[ev$ISIN == k, 1]
    adhocs = as.POSIXct(adhocs, "%Y-%m-%d %H:%M:%OS", tz="CET")
    adhocs = adhocs[!is.na(adhocs)]
    
    # only keep adhoc dates which lay in the interval of available tick data
    adhocs.relevant = adhocs[adhocs > v$TIMESTAMP[1]]
    adhocs.relevant = adhocs.relevant[adhocs.relevant < tail(v$TIMESTAMP, n=1)]
    
    if (length(adhocs.relevant) > 0) {
      # do magic stuff
      l = length(adhocs.relevant)
      offset.seconds = c(0, 1, 5, 10, 30, 60, 60*5, 60*10, 60*60)
      timestamps = matrix(rep(adhocs.relevant,length(offset.seconds)), nrow=l)
      offset = matrix(rep(offset.seconds, l), byrow=TRUE, nrow=l)
      timestamps.future = timestamps + offset
      
      fun = stepfun(v$TIMESTAMP, c(v$PRICE, tail(v$PRICE, n=1)), f=1, right=TRUE)
      
      # collect corresponding PRICE for each TIMESTAMP
      result = c()
      for (i in 1:l) {
        x = fun(timestamps.future[i,])
        result = rbind(result, x)
      }
      colnames(result) = paste("+", offset.seconds, "s", sep="")
      
      # calculate PRICE delta over time
      delta = result / result[,1]
      colnames(delta) = paste("s", offset.seconds, sep="")
      rownames(delta) = make.names(adhocs.relevant, unique=TRUE)

      keyval(key=cbind(isin=k, date=as.character(adhocs.relevant)), 
             val=delta)
    }
}

#debug(red.ticks)
data <- mapreduce(input="data/1017_01_M_08_E_20090331/monthly_bba_aa_20090331_small.csv",
                  input.format=inputformat,
                  map = map.ticks
                  ,reduce = red.ticks
                 )

data.df <- from.dfs(data)
# View(data.df)

tick.delta = data.df$val - 1.0
tick.isin = data.df$key[, 1]
tick.date = data.df$key[, 2]

offset.seconds = c(0, 1, 5, 10, 30, 60, 60*5, 60*10, 60*60)
offset.names = paste("+", offset.seconds, "s", sep="")
# View(tick.delta)
for (i in 1:length(data.df$key)) {
  jpeg(paste("plots/", tick.isin[i], ".jpg", sep=""))
  barplot(tick.delta[i,], 1:length(offset.names), names.arg=offset.names,
          ylab="BID price change in %", main = paste("ISIN:",tick.isin[i]),
          sub=paste("adHocMessage from:", as.POSIXlt(tick.date[i], origin = "1970-01-01")),
          ylim=c(-0.2,0.2))
  dev.off()
}
adhoc.count = dim(data.df$key)[1]

jpeg("plots/summary.jpeg")
barplot(colSums(tick.delta != 0.) / adhoc.count, 1:length(offset.names), names.arg=offset.names,
        ylab="% of stocks that show activity after an adHocMessage", main = "Trade activity after published adHoc message",
        sub=paste("mean over", adhoc.count, "adhoc messages"), ylim=c(0,1))
dev.off()

# again, debug
barplot(colSums(tick.delta != 0.) / adhoc.count, 1:length(offset.names), names.arg=offset.names,
        ylab="% of stocks that show activity after an adHocMessage", main = "Trade activity after published adHoc message",
        sub=paste("mean over", adhoc.count, "adhoc messages"), ylim=c(0,1))
