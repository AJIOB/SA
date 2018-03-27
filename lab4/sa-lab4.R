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

# Load the data as a corpus
docs <- Corpus(VectorSource(text))

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming (almost basic word form)
docs <- tm_map(docs, stemDocument)

# calculate word frequencies
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# word cloud building
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

