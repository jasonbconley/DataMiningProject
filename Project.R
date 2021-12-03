library(tm)
library(corpus)
library(SnowballC) # love lover lovingly lovely loving
library(Rcpp)
library(ggplot2)
library(wordcloud2)
library(dplyr) # https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

# Book: https://drive.google.com/file/d/1gn7cMdpMkDwHVTfDldAkn5i3_pRtoH-H/view 

# Here we set up the fraudulent email data set into a Corpus for processing
email_data <- read.csv("https://raw.githubusercontent.com/jasonbconley/DataMiningProject/main/fraud_email_.csv")

# Corpus functionality requires a column be named "text"
colnames(email_data) <- c("text", "class")
emailCorpusFrame <- corpus_frame(email_data)
emailCorpus <- Corpus(VectorSource(emailCorpusFrame$text))

# Here we strip the data of the things like whitespace and change all
# characters to lower case. Essentially so that the tm dataset can work with
# the data frame
emailCorpus <- tm_map(emailCorpus, tolower)

emailCorpus <- tm_map(emailCorpus, removeURL)

replaceComma <- function(x) gsub(",", " ", x)
emailCorpus <- tm_map(emailCorpus, replaceComma)

conjunctions <- c("a", "us", "may", "that", "and", "can", "get", "make", "shall", "are", "as", "at", "but", "for", "has", "the", "is", "to", "or", "this", "come", "also", "available", "via")
myStopwords <- c(stopwords("en"), conjunctions) # stopwords("en")
emailCorpus <- tm_map(emailCorpus, removeWords, myStopwords)

emailCorpus <- tm_map(emailCorpus, removePunctuation)

emailCorpus <- tm_map(emailCorpus, removeNumbers)

emailCorpus <- tm_map(emailCorpus, stripWhitespace)

emailCorpus <- tm_map(emailCorpus, stemDocument)

emailCorpusFrame <- as_corpus_frame(emailCorpus)
term_stats(emailCorpusFrame, ngrams=5)

# Now we can create the term document matrix, a collection of all terms and
# the number of their occurrences in each document or email
tdm <- TermDocumentMatrix(emailCorpus, control=list(wordLengths=c(2,Inf)))
tdm <- removeSparseTerms(tdm, 0.9)

# Creating a word cloud, we will use this to show the user the most frequent words
m <- as.matrix(tdm)
wordFreq <- sort(rowSums(m), decreasing=TRUE)
set.seed(123)
wordcloud2(data=enframe(wordFreq[1:20]), ellipticity = 1, minRotation = -pi/6, maxRotation = -pi/6)

# Association rule code
totalEmails <- nrow(emailCorpusFrame)
allEmails <- as.vector(emailCorpusFrame$text)

#getSupport <- function(wordChoices, suppCount) {
#  supportNumber <- 0
#  
#  for (email in allEmails) {
#    found = TRUE
#    for (word in wordChoices) {
#      check <- grepl(word, email, ignore.case = TRUE)
#      if (check == 0) {
#        found = FALSE
#        break
#      }
#    }
#    if (found) {
#      supportNumber = supportNumber + 1
#    }
#  }
#  
#  supportNumber
# }
#

# Cluster terms and plot
distMatrix <- dist(scale(m))
fit <- hclust(distMatrix, method = "ward.D") # Will go into this in the presentation, hierarchical clustering, not using Ward (1963) clustering criterion
plot(fit, xlab = "Words", sub = "Using Ward Clustering")
rect.hclust(fit, k=10)

# groups <- cutree(fit, k=10)
# group_frame <- data.frame(names(groups), groups[names(groups)])
# colnames(group_frame) <- c("word", "group")
# k = 10
# currentFrame <- data.frame(1:k)
# for (indx in 1:k) {
#   group <- (filter(group_frame, group == k))$words
#   currentWords = group$word
# }

grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )

# sapply("mrkdwn.Rmd", knit, quiet = T)

save(emailCorpusFrame, fit, tdm, wordFreq, grayLevels, totalEmails, allEmails, file = "objects.RData")

