shiny::runApp(system.file("shiny/", package="googleVis"))
# server.R
library(googleVis)

shinyServer(function(input, output) {
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  output$view <- renderGvis({
    gvisScatterChart(datasetInput(),
                     options=list(title=paste('Data:',input$dataset)))
  })
})

# ui.R
shinyUI(pageWithSidebar(
  headerPanel("googleVis on Shiny"),
  sidebarPanel(
    selectInput("dataset", "Choose a dataset:",
                choices = c("rock", "pressure", "cars"))
  ),
  mainPanel(
    htmlOutput("view")
  )
))