Sys.setenv(HADOOP_CMD="/usr/local/hadoop/bin/hadoop")
library(rhdfs)
hdfs.init()

setwd("DeutscheBoerse")
input.names = list.files(path = ".", pattern=".*zip")

# create new directory on HDFS
hdfs.mkdir("/user/isresearch/Data")

getindex(input.names, name)
# push files to HDFS. Only necessary once!
counter = 0
for (name in input.names) {
  counter = counter + 1
  print(paste(counter, name))
  
  # unzip file and read name of extracted file
  unzip(name, exdir = "tmp/")
  source.name = list.files(path = "tmp")
  
  path.source = paste(getwd(), "/tmp/", source.name, sep="")
  path.dest = paste("/user/isresearch/Data/", source.name, sep="")
  # load to HDFS
  hdfs.put(src=path.source, dst=path.dest)
  
  # do not keep large unzipped file
  unlink(paste(getwd(), "/tmp", sep=""), recursive=TRUE)
}



