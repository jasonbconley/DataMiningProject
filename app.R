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

load("objects.RData")

pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:4)]

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
    sidebarLayout(
        mainPanel(
           titlePanel("Most Popular Words in the dataset"),
           plotOutput("wordCloud")
        ),
        sidebarPanel (
          pickerInput("wordCloudChoices","Word Cloud Choices", selected = names(wordFreq), choices=names(wordFreq), options = list(`actions-box` = TRUE),multiple = T),
          pickerInput("wordChoices","Association Rule", selected = "will", choices=names(wordFreq), options = list(`actions-box` = TRUE),multiple = T),
          # numericInput("supportCount", "Support Count [0, 1]", value = 0.1, step = 0.1, min = 0.1, max = 0.9),
          submitButton("Submit"),
          helpText("Percentage of emails containing the chosen word(s): "),
          textOutput("numSupported")
        )
    ),
    verticalLayout (
        mainPanel(
          helpText("This page was created for a Data Mining Class and displays the word cloud and association rule outputs from the popular Nigerian Letter Email Fraud dataset")
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
      out <- as.String(round(100 * (supportNumber / totalEmails), digits = 2))
      out <- out + "% of emails contain the chosen association rule"
      out 
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
