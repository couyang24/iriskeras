library(tidyverse)
library(caret)
# library(data.table)

result <- read_csv("my_preds.csv",col_names = c("number","rate"), skip = 1)

i = 1
# result %>% tail()

list <- list()

for (i in 1:30){
    max = max(result[(i-1)*3+1,2], result[(i-1)*3+2,2], result[(i-1)*3+3,2])
    list[i] <- if_else(max == result[(i-1)*3+3, 2], 3, if_else(max == result[(i-1)*3+2,2], 2, 1))
}

list %>% rbind()

confusionMatrix()
data(iris)

lapply(iris, class)
