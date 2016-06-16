Sys.setenv(HADOOP_CMD="/usr/local/hadoop/bin/hadoop")
Sys.setenv(HADOOP_STREAMING="/usr/local/hadoop/share/hadoop/tools/lib/hadoop-streaming-2.4.1.jar")
options(stringsAsFactors = FALSE) 
library(rhdfs)
hdfs.init()

library(rmr2)

# load isin_set
setwd("/home/isresearch")
ev = read.csv("AxelPerschmann/events.csv", sep = ",", as.is=TRUE, col.names=c("date", "ISIN"))
isin.list = as.character(unique(ev[,2]))

### READ TICKS
map.ticks <- function(k, v) {
  # Skip header lines
  v = v[v$PRICE != "PRICE",]
  
  # filter input
  # v = v[v$BID_ASK_FLAG == 'A', ]
  
  # type conversion to double only necessary for v$PRICE, because we do n ot need v$HSEC and v$UNITS
  v$PRICE = as.double(v$PRICE)
  v = v[v$PRICE != 0.0, ] # erroneous values in data?!
  
  # only extract ISIN's when we have observed an AdHoc msg
  value = cbind(v, useful=v$ISIN %in% isin.list)
  value = value[value$useful == TRUE,]
  
  keyval(key=paste(value$ISIN, value$BID_ASK_FLAG, sep="_"),
         val=subset(value, select=c("TIMESTAMP", "PRICE")))
}

red.ticks <- function(k, v) {
  v$TIMESTAMP = as.POSIXct(v$TIMESTAMP, "%Y-%m-%d %H:%M:%OS", tz="CET")
  v = v[order(v$TIMESTAMP),]
  
  adhocs = ev[ev$ISIN == unlist(strsplit(k, "_"))[1], 1]
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

inputformat <- make.input.format("csv", sep = ";", stringsAsFactors = FALSE,
                                 col.names=c("WKN", "ISIN", "INSTRUMENT_NAME", "TIMESTAMP", "HSEC", "PRICE", "UNITS", "BID_ASK_FLAG") )

rmr.options(backend="hadoop")
files = hdfs.ls("Data_sub")[,6]
files = files[grep(".*csv", files)]
# files = files[1:1]
hdfs.delete("/user/isresearch/output.csv")
# hdfs.init()
t1 = proc.time()

data<- mapreduce(input=files
                 ,input.format=inputformat
                 ,output="user/isresearch/output.csv"
                 ,output.format=make.output.format("csv", sep=";")
                 ,map = map.ticks
                 ,reduce = red.ticks
)
t2 = proc.time()
data.df = from.dfs(data, format=make.input.format("csv", sep=";"))

# ensure correct formatting
data.df$val[1] <- lapply(data.df$val[1], as.character)
data.df$val[2] <- lapply(data.df$val[2], as.character)
data.df$val[3:11] <- lapply(data.df$val[3:11], as.numeric)

paste("Time consumed by MapReduce: ", (t2-t1)[3], "s", sep="")

tick.isin = unlist(data.df$val[1])
tick.date = unlist(data.df$val[2])
tick.values = data.df$val[3:11]
tick.delta =  tick.values - 1.0

offset.seconds = c(0, 1, 5, 10, 30, 60, 60*5, 60*10, 60*60)
offset.names = paste("+", offset.seconds, "s", sep="")
# View(tick.delta)
for (i in 1:length(tick.date)) {
  jpeg(paste("AxelPerschmann/plots/", tick.isin[i], ".jpg", sep=""))
  barplot(unlist(tick.delta[i,]), 1:length(offset.names), names.arg=offset.names,
          ylab="BID price change in %", main = paste("ISIN:",tick.isin[i]),
          # sub=paste("adHocMessage from:", as.POSIXlt(tick.date[i], origin = "1970-01-01")),
          sub=paste("adHocMessage from:", tick.date[i]),
          ylim=c(-0.2,0.2))
  dev.off()
}

adhoc.count = length(tick.date)
jpeg("AxelPerschmann/plots/summary.jpeg")
barplot(colSums(tick.delta != 0.) / adhoc.count * 100, 1:length(offset.names), names.arg=offset.names,
        ylab="% of stocks that show activity after an adHocMessage", main = "Trade activity after published adHoc message",
        sub=paste("mean over", adhoc.count, "adhoc messages"))
dev.off()
