library(shiny)
library(leaflet)

ui <- fluidPage(
  leafletOutput("map1"),
  fluidRow(
    column(6, actionButton("addHeat", label = "Add Heatmap")),
    column(6, actionButton("rmHeat", label = "Rm Heatmap"))
  ),
  fluidRow(
    column(6, textOutput("message")),
    column(6,  textOutput("features")))
)

server <- function(input, output, session) {

  v <- reactiveValues(msg = "")
  rndPoints <- data.frame(ID = 1:10,
                          X =  runif(n = 10,  min = 0, max = 45),
                          Y =  runif(n = 10,  min = 0, max = 45))


  output$map1 <- renderLeaflet({
    leaflet(data = rndPoints) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~X, lat = ~Y, radius = 5) %>%
    addDrawToolbar(
      layerID = "userDrawn",
      polyline = FALSE,
      circle = FALSE,
      marker = FALSE,
      edit = FALSE,
      polygon = FALSE,
      rectangle = TRUE,
      remove = TRUE,
      singleLayer = FALSE
    )
  })

  output$message <- renderText({
    v$msg
  })

  output$features <- renderPrint({
    v$features
  })


  observe({
    input$map1_userDrawn_deleting

    prox <- leafletProxy("map1", data =  rndPoints)

    if (isTRUE(input$map1_userDrawn_deleting)){
      prox %>% removeHeatmap("test")
    } else if (is.null(input$map1_userDrawn_deleting)){
      prox %>% addHeatmap( lng = ~X, lat = ~Y, layerId = "test")
    }
  })

  observeEvent(input$addHeat,{
    prox <- leafletProxy("map1", data = rndPoints)
    prox %>%
      addHeatmap(lng = ~X,  lat = ~Y,  layerId = "test")
  })

  observeEvent(input$rmHeat,{
    prox <- leafletProxy("map1")
    prox %>%
      removeHeatmap("test")
  })





  observeEvent(input$map1_userDrawn_created, {
    v$msg <- paste("Drawn created :\n", input$map1_userDrawn_created)
  })

  observeEvent(input$map1_userDrawn_features, {
    v$features <- paste("Current features :\n", input$map1_userDrawn_features)
  })

  observeEvent(input$map1_userDrawn_deleted, {
    v$msg <- paste("Drawn deleted :\n", input$map1_userDrawn_deleted)
  })

  observeEvent(input$map1_userDrawn_deleting, {
    v$msg <- paste("Deleting :\n", input$map1_userDrawn_deleting)
  })

  observeEvent(input$map1_userDrawn_editing, {
    v$msg <- paste("Deleting :\n", input$map1_userDrawn_editing)
  })

}

shinyApp(ui, server)
