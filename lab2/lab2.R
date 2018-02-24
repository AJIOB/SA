#set working directory
setwd("D:/labs/SA/lab2/")

confidential_interval = 0.95

#read data
data <- read.table("input.data", sep = "" , header = F, na.strings ="", stringsAsFactors= F)

mpg <- data[, 1]
acceleration <- data[, 4]

# mathematical expectation
M_mpg <- mean(mpg)
M_acceleration <- mean(acceleration)

# dispersion (variation)
D_mpg <- var(mpg)
D_acceleration <- var(acceleration)

# Критерий Фишера
Fisher <- var.test(mpg, acceleration, conf.level = confidential_interval)

# Критерий Стьюдента
t <- t.test(mpg, acceleration, conf.level = confidential_interval)
