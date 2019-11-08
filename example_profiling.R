library(microbenchmark) # библиотека для сравнения скорости работы


# делаем довольно много данных
data(iris) 
long_iris = iris[rep(seq_len(nrow(iris)), 1000), ] 



# генерируем набор случайных индексов для сабсета
set.seed(123)
indices = sample(nrow(long_iris), 10000, replace = TRUE)


for(i in 1:100) long_iris[indices, ]

`[.data.frame`

fast_subset = function(df, index){
    res = lapply(df, "[", index)
    class(res) = "data.frame"
    rownames(res) = seq_along(res[[1]]) # нужна, иначе кол-во строк не будет определятся
    res
}
    

temp1 = long_iris[indices, ]
temp2 = fast_subset(long_iris, indices)
all(temp1 == temp2)    

microbenchmark(
    subset = long_iris[indices, ],
    fast = fast_subset(long_iris, indices)
    
)

# Unit: microseconds
# expr      min        lq     mean    median        uq      max neval
# subset 1000.499 1043.3720 1269.918 1076.4790 1191.8575 15373.31   100
# fast    615.131  653.0385 1007.292  687.1395  729.8475 14622.77   100

























































microbenchmark(
    true = unlist(long_iris, use.names = TRUE),
    false = unlist(long_iris, use.names = FALSE)
)

# Unit: milliseconds
# expr        min         lq      mean     median         uq      max neval
# true 289.000975 316.053991 358.94059 348.747639 382.230065 504.9388   100
# false  1.276281   1.435527  14.44776  2.506377   2.968387 156.2049   100