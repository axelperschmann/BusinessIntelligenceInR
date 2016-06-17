data.df = from.dfs("/user/isresearch/output.csv", format=make.input.format("csv", sep=";"))

# # ensure correct formatting
# data.df$val[1] <- lapply(data.df$val[1], as.character)
# data.df$val[2] <- lapply(data.df$val[2], as.character)
# data.df$val[3:11] <- lapply(data.df$val[3:11], as.numeric)

tick.isin = unlist(data.df$val[1])
tick.date = unlist(data.df$val[2])
tick.values = data.df$val[3:11]

offset.names = c("+0s", "+1s", "+5s", "+10s", "+30s", "+1m", "+5m", "+10m", "+1h")

# trade activity after publicatoin of an adHoc message
adhoc.count = length(tick.date)
jpeg("AxelPerschmann/plots/summary.jpeg")
bplt <- barplot((rbind(colSums(tick.values != 1.) / adhoc.count * 100)), 1:length(offset.names), ylim=c(0,30), width=0.1, space=0.2,
                ylab="%", main = "Trade activity after publication of adhoc message", col=c("grey", "darkgreen", "red"), density=c(25, 15, 15), angle=c(0, 45, -45),
                sub=paste("ratio over a total of", adhoc.count, "adhoc messages"), axisnames=FALSE, 
                legend=c("overall", "positiv performing", "negativ performing"), args.legend = list(x = "topleft"))
par(new=TRUE)
bplt <- barplot(t(rbind(colSums(tick.values > 1.) / adhoc.count * 100)), 1:length(offset.names), ylim=c(0,30),
                col=c("darkgreen"), density=10, angle=c(45), axisnames=FALSE, width=0.1, space=0.2, beside=TRUE)

par(new=TRUE)
bplt <- barplot(t(rbind(colSums(tick.values < 1.) / adhoc.count * 100)), 1:length(offset.names), ylim=c(0,30),
                col=c("red"), density=10, angle=c(-45), axisnames=FALSE, width=0.1,  space=0.2, beside=TRUE)
axis(1, at = bplt, labels = offset.names, cex.axis = 0.8)
text(x= bplt, y= 1 + (colSums(tick.values != 1.) / adhoc.count * 100), labels=colSums(tick.values != 1.), xpd=TRUE)
dev.off()


means.pos = c()
means.neg = c()
means.all = c()
for(i in 2:length(offset.names)) {
  values = (tick.values[,i])
  
  # negative performance
  abs = values[values < 1.]
  x<-quantile(abs,c(0.01,0.99)) # exclude some nasty outliers
  abs <- abs[abs >=x[1] & abs<=x[2]]
  means.neg = cbind(means.neg, mean(abs))
  
  abs = values[values > 1.]
  x<-quantile(abs,c(0.01,0.99)) # exclude some nasty outliers
  abs <- abs[abs >=x[1] & abs<=x[2]]
  means.pos = cbind(means.pos, mean(abs))
  
  abs = values[values != 1.]
  x<-quantile(abs,c(0.01,0.99)) # exclude some nasty outliers
  abs <- abs[abs >=x[1] & abs<=x[2]]
  means.all = cbind(means.all, mean(abs))
  
}
jpeg("AxelPerschmann/plots/summary_performance.jpeg")
range = c(-4, 4)
bplt <- barplot(rbind((means.pos*100)-100), 2:length(offset.names), ylim=range, axisnames=FALSE, width=0.1, space=0.2,
                ylab="%", main = "Mean performance after publication of adhoc message", col=c("darkgreen", "red", "black"),
                sub=paste("mean over all samples with non-zero performance"), density=c(25, 25, 100), angle=c(45, -45, 0),
                legend=c("positiv performing", "negativ performing", "overall"), args.legend = list(x = "topleft"))
par(new=TRUE)
bplt <- barplot(rbind((means.neg*100)-100), 2:length(offset.names), ylim=range, axisnames=FALSE, width=0.1, space=0.2,
                density=25, angle=-45, col="red")
par(new=TRUE)
bplt <- barplot(rbind((means.all*100)-100), 2:length(offset.names), ylim=range, col="black",axisnames=FALSE, width=0.1, space=0.2,
                border="black")
abline(h=0, lwd = 2)
axis(1, at = bplt, labels = offset.names[-1], cex.axis = 0.8)

text(x= bplt, y= 0.2 + (means.pos*100)-100, labels=paste(round((means.pos*100)-100, digits=2), "%", sep=""), cex=0.8)
text(x= bplt, y= - 0.2 + (means.neg*100)-100, labels=paste(round((means.neg*100)-100, digits=2), "%", sep=""), cex=0.8)
text(x= bplt, y= -0.25, labels=paste(round((means.all*100)-100, digits=2), "%", sep=""), xpd=TRUE, cex = 0.8)
dev.off()

# View(tick.delta)
pb <- txtProgressBar(min=1, max = length(tick.date), style = 3)
for (i in 1:length(tick.date)) {
  setTxtProgressBar(pb, i)
  jpeg(paste("AxelPerschmann/plots/individuals/", tick.isin[i], ".jpg", sep=""))
  barplot(unlist(tick.values[i,] - 1), 1:length(offset.names), names.arg=offset.names,
          ylab="BID price change in %", main = paste("ISIN:",tick.isin[i]),
          # sub=paste("adHocMessage from:", as.POSIXlt(tick.date[i], origin = "1970-01-01")),
          sub=paste("adHocMessage from:", tick.date[i]),
          ylim=c(-0.2,0.2))
  dev.off() 
}

