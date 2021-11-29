# Utilizing code and information from 
# https://drive.google.com/file/d/1gn7cMdpMkDwHVTfDldAkn5i3_pRtoH-H/view
# a data mining book with a focus on the R programming language
install.packages("tm")
install.packages("corpus")
install.packages("SnowballC")
library(tm)
library(corpus)
library(SnowballC)

# Here we set up the fraudulent email dataset into a Corpus for processing
# A corpus is a data structure that I had not seen before this text. Running
# the command View(emailCorpus) after creating it will show the structure
email_data <- read.csv("https://raw.githubusercontent.com/jasonbconley/DataMiningProject/main/fraud_email_.csv")
colnames(email_data) <- c("text", "class")
emailCorpusFrame <- corpus_frame(email_data)
emailCorpus <- Corpus(VectorSource(emailCorpusFrame$text))

# Here we strip the data of the things like whitespace and change all
# characters to lower case. Essentially so that the tm dataset can work with
# the data frame
emailCorpus <- tm_map(emailCorpus, tolower)
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
emailCorpus <- tm_map(emailCorpus, removeURL)
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
emailCorpus <- tm_map(emailCorpus, removeNumPunct)
replaceComma <- function(x) gsub(",", " ", x)
emailCorpus <- tm_map(emailCorpus, replaceComma)
emailCorpus <- tm_map(emailCorpus, removePunctuation)
emailCorpus <- tm_map(emailCorpus, removeNumbers)
emailCorpus <- tm_map(emailCorpus, removeWords)
emailCorpus <- tm_map(emailCorpus, stripWhitespace)

# Now we can create the term document matrix, a collection of all terms and
# the number of their occurrences in each document or email
tdm <- TermDocumentMatrix(emailCorpus, control=list(wordLengths=c(1,Inf)))
tdm

# This function shows the frequency of terms that are greater than 10
findFreqTerms(tdm, lowfreq = 10)
