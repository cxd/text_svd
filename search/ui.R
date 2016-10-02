library(shiny)

shinyUI(
  fluidPage(
    titlePanel("Search Requirements"),
    mainPanel(
      textInput("querytext", "Search Query", "the text to search..."),
      numericInput("maxResults", "Max Results", 10, min=5, max=100),
      selectInput("projectFilter", "Filter Project", choices="None", selectize=TRUE, multiple=TRUE),
      submitButton("Search"),
      tableOutput("searchResults")
    )
  )
)