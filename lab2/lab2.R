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

# quantiles
quantile(replicate(length(mpg), mean(sample(mpg, rep=TRUE))), c(1 - confidential_interval, confidential_interval))
quantile(replicate(length(acceleration), mean(sample(acceleration, rep=TRUE))), c(1 - confidential_interval, confidential_interval))

# �������� ������
var.test(mpg, acceleration, conf.level = confidential_interval)

# �������� ���������
t.test(mpg, acceleration, conf.level = confidential_interval)
