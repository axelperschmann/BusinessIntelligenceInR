library(rmr2)
rmr.options(backend = "local")
data <- mapreduce(input = "loremipsum.txt", 
               input.format = "text",
               map = function(k,lines){
                 keyval(
                   key = unlist(strsplit(lines, split = " ")),
                   val = 1)
               },
               reduce = function(word, counts){
                 keyval(key = word,
                        val = sum(counts))
               }
)
result = from.dfs(data)

result
