#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
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
conjunctions <- c("a", "us", "may", "that", "and", "can", "get", "make", "shall", "are", "as", "at", "but", "for", "has", "the", "is", "to", "or", "this", "come", "also")
myStopwords <- c(stopwords("en"), "available", "via", conjunctions)
emailCorpus <- tm_map(emailCorpus, removeWords, myStopwords)
emailCorpus <- tm_map(emailCorpus, removePunctuation)
emailCorpus <- tm_map(emailCorpus, removeNumbers)
emailCorpus <- tm_map(emailCorpus, stripWhitespace)

# Now we can create the term document matrix, a collection of all terms and
# the number of their occurrences in each document or email
tdm <- TermDocumentMatrix(emailCorpus, control=list(wordLengths=c(2,Inf)))
tdm <- removeSparseTerms(tdm, 0.9)

pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:4)]

# Creating a word cloud, we will use this to show the user the most frequent words
m <- as.matrix(tdm)
wordFreq <- sort(rowSums(m), decreasing=TRUE)
set.seed(123)
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )

# Association rule code
totalEmails <- nrow(emailCorpusFrame)
allEmails <- as.vector(emailCorpusFrame$text)

getSupport <- function(wordChoices, suppCount) {
  supportNumber <- 0
  
  for (email in allEmails) {
    found = TRUE
    for (word in wordChoices) {
      check <- grepl(word, email, ignore.case = TRUE)
      if (check == 0) {
        found = FALSE
        break
      }
    }
    if (found) {
      supportNumber = supportNumber + 1
    }
  }
  
  supportNumber
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Nigerian Letter Email Fraud"),
    tags$head(tags$style(".leftAlign{float:left;}")),
    verticalLayout(
        mainPanel(
           titlePanel("Most Popular Words in the dataset"),
           plotOutput("wordCloud")
        ),
        wellPanel (
          pickerInput("wordCloudChoices","Word Cloud Choices", selected = names(wordFreq), choices=names(wordFreq), options = list(`actions-box` = TRUE),multiple = T),
          pickerInput("wordChoices","Association Rule", selected = "will", choices=names(wordFreq), options = list(`actions-box` = TRUE),multiple = T),
          # numericInput("supportCount", "Support Count [0, 1]", value = 0.1, step = 0.1, min = 0.1, max = 0.9),
          submitButton("Submit"),
          helpText("Number of emails supported by given association rule choices: "),
          textOutput("numSupported")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$wordCloud <- renderPlot({
      wordcloud(words=input$wordCloudChoices, freq=wordFreq[input$wordCloudChoices], random.order=F, colors=pal)
    })
    
    output$numSupported <- renderText({
      supportNumber = getSupport(input$wordChoices, input$supportCount)
      out <- as.String(round(supportNumber / totalEmails, digits = 2))
      out <- out + "% of emails contain the chosen association rule"
      out 
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
