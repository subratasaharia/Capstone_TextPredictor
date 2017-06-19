#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tm)
library(dplyr)
library(RWeka)
library(wordcloud)
library(RColorBrewer)

# Load Predict/StringSearch functions 
source("Predictv5.R")


# Shiny UI

shinyUI(fluidPage(
  titlePanel("Text Predictor"),
  sidebarLayout(
    sidebarPanel(
      h4("Choose configuration options:"),
      h5("How many prediction outputs to be provided?"),
      selectInput('count', 'Count', c(1,3,5,10)),
      
      h5("Should the phrase be shown along with predicted word?"),
      h6("Select Phrases if you want to show:"),
      checkboxInput('phrases', 'Phrases',value=FALSE),
      br(),
      
      strong("A few points before we kick off!"),
      br(),
      strong("So, what can the App accomplish?"),
      em("The app "),strong("predicts"),em("the next word in the sentence you are typing. It can 
        also"),strong("complete"),em("the word you are typing depending on where you 
        stop typing."),
      br(),
      br(),
      strong("How to leverage the App?"),
      p("In case you want it to predict the next word in the sentence:-"),
      em("Type 1/2/3/.. words and"),
      strong("press blank space"),
      em ("for the App to predict the next word"),
      br(),
      p("In case you want it to complete the word you are typing:-"),
      em("Type 1/2/3/.. half words and"),
      strong("do not press"),
      em("blank space for the App to complete the word"),
      br(),
      h6("Developed by: Subrata Saharia")
      ),
    
    mainPanel(
      h4("Enter text of your choice:"),
      textInput("text", label="", value=""),
      br(),
      h4("Predicted word(s)  is(are):"),
      verbatimTextOutput("TextPrediction"),
      strong("Have a look at the word cloud!"),
      p("The word cloud shows the top predicted words, bigger
        the font, higher the significance in prediction!"),
      plotOutput("plot")
      
  )
  )))