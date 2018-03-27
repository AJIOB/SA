#set working directory
setwd("D:/labs/SA/lab4/")

# Install from CRAN
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

folder_delim <- "/"

input_folder <- "input"
file_extension <- ".txt"

group1 <- "hardware"
group2 <- "programming"
group3 <- "economics"

# load the text
file_path <- paste(c(input_folder, group1, paste("1", file_extension, sep = "")), collapse = folder_delim)
text <- readLines(file_path)
