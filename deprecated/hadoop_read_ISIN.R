library(rmr2)
rmr.options(backend="local")
#  Seconds include HSEC
options(digits.secs=2)

red.isin = function(k,v) {
  res = ""

  keyval(cbind(k, res), k)
}
debug(red.isin)

inputformat <- make.input.format("csv", sep = ",", col.names=c("date", "isin"))
isin <- mapreduce(input="events.csv",
                  input.format = inputformat,
                  map = function(k, v) {
                    keyval(key=v[2],
                           val=1)
                  }
                  ,reduce = red.isin
                  )
isin.df <- from.dfs(isin)
View(isin.df)