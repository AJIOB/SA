#set working directory
setwd("D:/labs/SA/lab1/")

#read data
data <- read.table("input.data", sep = "" , header = F, na.strings ="", stringsAsFactors= F)

mpg <- data[, 1]
acceleration <- data[, 6]

plot(mpg, acceleration, xlab = "mpg", ylab = "acceleration", title("Link: \"http://archive.ics.uci.edu/ml/datasets/Auto+MPG\""))

#correlation coefficient
r <- cor(mpg, acceleration)

#linear regression
abline(lm(acceleration ~ mpg))

