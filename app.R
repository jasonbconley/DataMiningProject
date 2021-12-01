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

ui <- navbarPage("Navbar",
        tabPanel("WordCloud",
          sidebarLayout(
                sidebarPanel(
                  pickerInput("wordCloudChoices","Word Cloud Choices", selected = names(wordFreq), choices=names(wordFreq), options = list(`actions-box` = TRUE),multiple = T),
                  pickerInput("wordChoices","Word Choice", selected = "will", choices=names(wordFreq), options = list(`actions-box` = TRUE),multiple = T),
                  helpText("Percentage of emails containing the chosen word(s): "),
                  textOutput("numSupported"),
                  submitButton("Submit")
                ),
                mainPanel(
                  plotOutput("wordCloud")
                )
             )
          ),
        tabPanel("Clustering",
          verticalLayout(
            mainPanel(
              plotOutput("Dendrogram", inline=TRUE)
            ),
            wellPanel(
              helpText("Choose grouping size: "),
              numericInput("k", "Cluster Group Size", value = 10, min=3, max=10, step=1),
              submitButton("Submit"),
              htmlOutput("groups")
            )
          )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$wordCloud <- renderPlot({
      wordcloud(words=input$wordCloudChoices, freq=wordFreq[input$wordCloudChoices], random.order=F, colors=pal)}, height = 600, width = 600
    )
    
    output$numSupported <- renderText({
      supportNumber = getSupport(input$wordChoices, input$supportCount)
      out <- as.String(round(100 * (supportNumber / totalEmails), digits = 2))
      out <- out + "% of emails contain the chosen association rule"
      out 
    })
    
    output$Dendrogram <- renderPlot({
      plot(fit, cex=input$opt.cex, cex.lab=input$opt.cexaxis)
      rect.hclust(fit, k=input$k)}, height = 600, width = 1400
    )
    
    output$groups <- renderUI({
      
      x <- paste0("<strong>Here are your states</strong>: ", paste(states, collapse = " "))
      HTML(x)
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
