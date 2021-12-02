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
library(wordcloud2)
library(dplyr)
library(tidyverse)
library(knitr)
library(rmarkdown)
library(NLP)
library(RColorBrewer)

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

sapply(rmdFile, knit, quiet = T)

ui <- navbarPage("Mining the \"419\" Dataset",
        tabPanel("Main",
          verticalLayout(
            includeMarkdown(rmdFile)
          )       
        ),
        tabPanel("WordCloud",
          verticalLayout(
                fluidRow(
                  column(8, align = "center", wordcloud2Output("wordCloud"))
                ),
                wellPanel(
                  pickerInput("wordCloudChoices","Word Cloud Choices", selected = names(wordFreq), choices=names(wordFreq), options = list(`actions-box` = TRUE),multiple = T),
                  pickerInput("wordChoices","Word Choice", choices=names(wordFreq), options = list(`actions-box` = TRUE),multiple = T),
                  helpText("Percentage of emails containing the chosen word(s): "),
                  verbatimTextOutput("numSupported", placeholder = TRUE),
                  submitButton("Submit"),
                  tags$head(tags$style("#numSupported{
                                  font-size: 20px;
                                 }"
                  )
                  )
                )
             )
          ),
        tabPanel("Clustering",
          verticalLayout(
            mainPanel(
              plotOutput("Dendrogram", inline=TRUE)
            ),
            wellPanel(
              helpText("K-Means Clustering: "),
              numericInput("k", "Choose a K value", value = 10, min=3, max=20, step=1),
              verbatimTextOutput("groups"),
              submitButton("Submit")
            )
          )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$main <- renderText({
     mainText
  })  
  
  output$wordCloud <- renderWordcloud2({
      wordcloud2(data=enframe(wordFreq[input$wordCloudChoices]), color = pal, ellipticity = 1, minRotation = -pi/6, maxRotation = -pi/6)}
    )
    
    output$numSupported <- renderText({
      supportNumber = getSupport(input$wordChoices, input$supportCount)
      out <- as.String(round(100 * (supportNumber / totalEmails), digits = 2))
      out <- out + as.String("% of emails (or ")
      out <- out + as.String(supportNumber)
      out <- out + as.String(" emails) contain the chosen word combination (total emails = ")
      out <- out + as.String(totalEmails) + as.String(")")
      out 
    })
    
    output$Dendrogram <- renderPlot({
      plot(fit, cex=input$opt.cex, cex.lab=input$opt.cexaxis, xlab = "Words", sub = "Using Ward Clustering")
      rect.hclust(fit, k=input$k)}, height = 600, width = 1400
    )
    
    output$groups <- renderText({
      groups <- cutree(fit, input$k)
      group_frame <- data.frame(names(groups), groups[names(groups)])
      colnames(group_frame) <- c("word", "group")
      x <- as.String("Words Grouped from 1:K\n")
      
      for (indx in 1:input$k) {
        group <- (filter(group_frame, group == indx))$word
        x <- x + "Group #" + as.String(indx) + ": "
        x <- x + as.String(paste(group, collapse = ", ")) 
        x <- x + as.String("\n")
      }
      x
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
