# Utilizing code and information from 
# https://drive.google.com/file/d/1gn7cMdpMkDwHVTfDldAkn5i3_pRtoH-H/view
# a data mining book with a focus on the R programming language
# Only have run the below install functions once
install.packages("tm")
install.packages("corpus")
install.packages("SnowballC")
install.packages("ggplot2")
install.packages("Rcpp")
install.packages("wordcloud")
# Will most likely have to run below library functions every time project
# is reopened
library(tm)
library(corpus)
library(SnowballC)
library(Rcpp)
library(ggplot2)
library(wordcloud)

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
conjunctions <- c("a", "that", "and", "are", "as", "at", "but", "for", "has", "the", "is", "to", "or", "this", "come", "also")
myStopwords <- c(stopwords("en"), "available", "via", conjunctions)
emailCorpus <- tm_map(emailCorpus, removeWords, myStopwords)
emailCorpus <- tm_map(emailCorpus, removePunctuation)
emailCorpus <- tm_map(emailCorpus, removeNumbers)
emailCorpus <- tm_map(emailCorpus, stripWhitespace)

# Now we can create the term document matrix, a collection of all terms and
# the number of their occurrences in each document or email
tdm <- TermDocumentMatrix(emailCorpus, control=list(wordLengths=c(2,Inf)))
tdm <- removeSparseTerms(tdm, 0.9)

# This function shows the frequency of terms that are greater than 2000
# findFreqTerms(tdm, lowfreq = 2000)

# Create a word cloud of the most popular words
termFrequency <- rowSums(as.matrix(tdm))
termFrequency <- subset(termFrequency, termFrequency>=2000)
df <- data.frame(term=names(termFrequency), freq=termFrequency)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") + xlab("Terms") + ylab("Count") + coord_flip()

# Setting some colors
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:4)]

# Creating a word cloud, we will use this to show the user the most frequent words
m <- as.matrix(tdm)
wordFreq <- sort(rowSums(m), decreasing=TRUE)
set.seed(123)
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3, random.order=F, colors=pal)