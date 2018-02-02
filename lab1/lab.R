#set working directory
setwd("D:/labs/SA/lab1/")

#read data
data <- read.table("input.data", sep = "" , header = F, na.strings ="", stringsAsFactors= F)

plot(data[, 1], data[, 6], xlab = "mpg", ylab = "acceleration", title("Link: \"http://archive.ics.uci.edu/ml/datasets/Auto+MPG\""))


