#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


# Shiny Server
shinyServer(function(input, output) {
  
  # Reactive functions  
  gettextforprediction <- reactive({
    textforprediction<-input$text
    if(textforprediction=="")
      textforprediction<-"Welcome"
    textforprediction
  })
  
  getcount<-reactive({
    countofwords<-as.numeric(input$count)
    countofwords
  })
  
  getphrases<-reactive({
    phrases<-input$phrases
    phrases
  })
  
  # Output functions
  
  output$TextPrediction <- renderText({
    String<-gettextforprediction()
    if(String=="Welcome"){
       words<-"Welcome to the Text Prediction App!" 
    }else{
        countofwords<-getcount()
        phrases<-getphrases()
        predictedword<-StringSearch(String, countofwords,phrases)
        predictedword<-predictedword[[1]]
        len<-length(predictedword)
        words<-NULL
        for(i in 1:len){
          words<-paste(words,predictedword[i],sep = "\n")
        }
    }
    words
  })
  
  output$plot<-renderPlot({
    String<-gettextforprediction()
    if(String=="Welcom"){
          predictedword<-StringSearch(String,1,FALSE)
    } else {
          countofwords<-getcount()
          phrases<-getphrases()
          predictedword<-StringSearch(String,countofwords,phrases)
    }
    wordcloud(predictedword[[1]],predictedword[[2]],
              rot.per = .2,colors = brewer.pal(12,"Paired"))
    
  })
})


