#set working directory
setwd("D:/labs/SA/lab4/")

# Install from CRAN
library("tm")
library("foreach")

folder_delim <- "/"

input_folder <- "input"
file_extension <- ".txt"

group1 <- list(name = "hardware", trade = c("1", "2", "3"), test = c("4", "5"))
group2 <- list(name = "programming", trade = c("6", "7", "8"), test = c("9", "10"))
group3 <- list(name = "economics", trade = c("11", "12", "13"), test = c("14", "15"))

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

parse_freqs <- function(text)
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
  data.frame(word = names(v),freq=v)
}

get_thesaurus <- function(group)
{
  text <- load_texts(group$name, group$trade)
  th <- parse_freqs(text)
  top <- head(th, thesaurus_size)
  as.vector(top$word)
}

voc <- NULL
voc$th1 <- get_thesaurus(group1)
voc$th2 <- get_thesaurus(group2)
voc$th3 <- get_thesaurus(group3)

calc_hits_in_voc <- function(thesuarus, spec_text)
{
  data_frame <- spec_text[spec_text$word %in% thesuarus,]
  sum(data_frame$freq)
}

get_hit_vector <- function(group_name, text_name)
{
  text <- load_text(group_name, text_name)
  freqs <- parse_freqs(text)

  test_num <- foreach (x = voc) %do% { 
    calc_hits_in_voc(x, freqs) 
    }
  unlist(test_num)
}

get_named_matrix <- function(values)
{
  A <- matrix(
    values,
    ncol = 3,
    byrow = FALSE)
  
  dimnames(A) <- list(
    NULL,         # row names 
    c(group1$name, group2$name, group3$name)  # column names 
  )
  A
}

trade_matrix <- get_named_matrix(
  c(group1$trade, group2$trade, group3$trade)
)

test_matrix <- get_named_matrix(
  c(group1$test, group2$test, group3$test)
)

# input:  one column - one group (columns must be named)
# output: one column - one thesaurus
get_hit_matrix <- function(group_matrix)
{
  res <- NULL

  for(col_name in colnames(group_matrix))
  {
    col <- group_matrix[, col_name]
    
    hits <- foreach (i = col) %do% {
      get_hit_vector(col_name, i)
    }
    
    hits <- unlist(hits)
    
    hits_matrix <- matrix(
      hits, 
      ncol = 3,
      byrow = TRUE
    )
    
    dimnames(hits_matrix) <- list(
      paste(col_name, col, sep = " "),         # row names 
      c("th1", "th2", "th3")  # column names 
    )
    
    res <- rbind(res, hits_matrix)
  }
  
  res
}

t <- get_hit_matrix(trade_matrix)
