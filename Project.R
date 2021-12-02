library(shiny)
library(shinyWidgets)
library(tm)
library(corpus)
library(SnowballC)
library(Rcpp)
library(ggplot2)
library(wordcloud)
library(dplyr)

# Here we set up the fraudulent email data set into a Corpus for processing
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
conjunctions <- c("a", "us", "may", "that", "and", "can", "get", "make", "shall", "are", "as", "at", "but", "for", "has", "the", "is", "to", "or", "this", "come", "also")
myStopwords <- c(stopwords("en"), "available", "via", conjunctions)
emailCorpus <- tm_map(emailCorpus, removeWords, myStopwords)
emailCorpus <- tm_map(emailCorpus, removePunctuation)
emailCorpus <- tm_map(emailCorpus, removeNumbers)
emailCorpus <- tm_map(emailCorpus, stripWhitespace)

# Now we can create the term document matrix, a collection of all terms and
# the number of their occurrences in each document or email
tdm <- TermDocumentMatrix(emailCorpus, control=list(wordLengths=c(2,Inf)))
tdm2 <- removeSparseTerms(tdm, 0.9)

# Creating a word cloud, we will use this to show the user the most frequent words
m <- as.matrix(tdm2)
wordFreq <- sort(rowSums(m), decreasing=TRUE)
set.seed(123)
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )

# Association rule code
totalEmails <- nrow(emailCorpusFrame)
allEmails <- as.vector(emailCorpusFrame$text)

# Cluster terms
distMatrix <- dist(scale(m))
fit <- hclust(distMatrix, method = "ward.D")

plot(fit, xlab = "Words", sub = "Using Ward Clustering")
rect.hclust(fit, k=10)
groups <- cutree(fit, k=10)
group_frame <- data.frame(names(groups), groups[names(groups)])
colnames(group_frame) <- c("word", "group")
k = 10
currentFrame <- data.frame(1:k)
for (indx in 1:k) {
  group <- (filter(group_frame, group == 1))$words
  currentWords = group$word
}

rmdFile <- c("mrkdwn.Rmd")

save(fit, tdm, wordFreq, grayLevels, totalEmails, allEmails, file = "objects.RData")

