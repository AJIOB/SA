#set working directory
setwd("D:/labs/SA/lab2/")

#read data
data <- read.table("input.data", sep = "" , header = F, na.strings ="", stringsAsFactors= F)

mpg <- data[, 1]
acceleration <- data[, 6]

#probabil
