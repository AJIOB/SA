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

thesaurus_size = 60

# read file
load_text <- function(group_name, file_name)
{
  # load the text
  file_path <- paste(c(input_folder, group_name, paste(file_name, file_extension, sep = "")), collapse = folder_delim)
  readLines(file_path)
}


# read first $thesausus_size words from file
load_texts <- function(group_name, file_names)
{
  text <- NULL
  for (fn in file_names)
  {
    text_temp <- load_text(group_name, fn)
    text <- paste(text, text_temp)
  }
  text
}

get_thesaurus <- function(text)
{
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
  head(d, thesaurus_size)$word
}

test <- load_texts(group1, c("1", "2", "3"))
q <- get_thesaurus(test)
