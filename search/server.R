library(shiny)

require(dplyr)
source("preprocess_text.R")
source("prepare_svd_document_model.R")
source("svd_search.R")



shinyServer(function (input, output, session) {
  
  queryInput <- reactive({
    input$querytext
    })
  
  maxResults <- reactive(input$maxResults)
  
  selectedFilter <- reactive({
    if (length(input$projectFilter) == 0) {
      c()
    } else if (input$projectFilter[1] == "None") {
      c()
    } else {
      c(input$projectFilter)
    }
    })
  
  select<- observe({
    updateSelectInput(session, "projectFilter", choices= c("None", projects))
    
  })
  
  output$searchResults <- renderTable({
    withProgress(message='Search in progress', {
      Sys.setlocale('LC_ALL','C') 
      query <- queryInput()
      max <- maxResults()
      filter <- selectedFilter()
      threshold <- 0.10
      outputBase <- "data/"
      dataSet <- search_text(query, c("item_key", "text"), outputBase, filter)
      dataSet <- dataSet[dataSet$cosine_dist >= threshold, ]
      if (nrow(dataSet) > max) {
        dataSet[1:max,]
      } else {
        dataSet
      }  
    })
  })
  
  
  
})