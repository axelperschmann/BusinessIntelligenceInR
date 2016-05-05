# enable fractional seconds
options(digits.secs=3)

filename = "data/1017_01_M_08_E_20090331/monthly_bba_aa_20090331.csv"

# read csv file
mydata = read.csv(filename, sep = ";", as.is=TRUE)
# only look at BID prices
mydata_filtered = mydata[mydata$BID_ASK_FLAG == "B",]
# get rid of annoying '0.0' values
mydata_filtered = mydata_filtered[mydata_filtered$PRICE != 0.0,]

pb <- txtProgressBar(min=1, max = length(isin_set), style = 3)
for (i in 15:length(isin_set)) {
  setTxtProgressBar(pb, i)
  is = isin_set[i]
  timeline = subset(mydata_filtered, ISIN==is)
  # print(paste(nrow(timeline),"ticks"))
  if (nrow(timeline) == 0) {
    # nothing to do here, move on to next ISIN
    next
  }
  
  # order timeline according to timestamps
  timeline = timeline[order(timeline$TIMESTAMP, timeline$HSEC),]
  
  # extract date, time and fractional seconds
  ts = paste(timeline[["TIMESTAMP"]], ".", timeline[["HSEC"]], sep="")
  ts = strptime(ts, "%Y-%m-%d %H:%M:%OS")
  # print first and last timestamp
  ts_first = head(ts, n=1)
  ts_last  = tail(ts, n=1)
  # print(paste(ts_first, "-", ts_last))
  
  # obtain all adHocMessage dates related to this isin
  adHoc_dates = strptime(subset(ev, isin==is)[,1], "%Y-%m-%d %H:%M:%OS")
  dates_in_range = c()
  
  # pick only adHoc_dates that lie within the range of our timeline
  for (j in 1:length(adHoc_dates)) {
    if (ts_first <= adHoc_dates[j] &&  adHoc_dates[j] <= ts_last) {
      dates_in_range = cbind(dates_in_range, as.numeric(adHoc_dates[j]))
    }
  }
  
  # plot only if at least one adHoc_date is within the range of our timeline
  if (length(dates_in_range) > 0) {
    jpeg(paste("plots/", is,  "rplot.jpg", sep=""))
    description = paste(ts_first, " - ", ts_last, " (", ts_last-ts_first, ")", sep="")
    
    price = timeline[["PRICE"]]
    plot(ts, price, type="S", main=paste("ISIN:",is), sub=description,
         ylab="BID price", xlab="time")
    points(ts, price)
    
    # mark adHoc date as a vertical line
    for (t in 1:length(dates_in_range)) {
      abline(v=dates_in_range[t], col="purple")
    }
    
    dev.off()
  }
}


events[1:5,2]
adHoc_dates = subset(ev, isin=="DE0007300402")
print(adHoc_dates)
timeline = subset(mydata, ISIN=="DE0007300402")
if (nrow(timeline) == 0) {
  stop('isin not found in dataset')
}

# sort stuff
library(dplyr)
timeline = arrange(data.frame(timeline), TIMESTAMP, HSEC)

# get rid of annoying '0.0' values
timeline = timeline[timeline$PRICE != 0,]

# extract date, time and fractional seconds
ts = paste(timeline[["TIMESTAMP"]], ".", timeline[["HSEC"]], sep="")
ts = strptime(ts, "%Y-%m-%d %H:%M:%OS")
# print first and last timestamp
print(paste(head(ts, n=1), "-", tail(ts, n=1)))

price = timeline[["PRICE"]]

# plot timeline
plot(ts, price, type="l")
abline(v=as.numeric(ts[10]), col="purple")

for (t in adHoc_dates[1]) {
  print(t)
  print(as.numeric(t))
  abline(v=as.numeric(t), col="purple")
}


for (i in 1:26) {
  print(ts[i])
}
print(adHoc_dates)


