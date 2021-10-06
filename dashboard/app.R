
library(shiny)

ui <- fluidPage(
  leafletOutput("mymap"),
  absolutePanel(
    numericInput("Latitud", "Latitud", 21.49,20.5,23,0.1),
    numericInput("Longitud", "Longitud", 104.89,103.5,106,0.1)
  )
  
)




server <- function(input, output){
  
  lat_point <- reactive({
    input$Latitud
  })
  
  lon_point <- reactive({
    input$Longitud
  })
  
  
  
  mexico <- readOGR(dsn = "./estados", layer = "states", encoding = "UTF-8")
  nayarit<-which(mexico@data$NOM_ENT=="Nayarit")
  map <- mexico@polygons[[nayarit]]
  pal <- colorBin("Reds",log(df$capacidad_personas))
  refugios_popup <- paste0("<strong>Municipio: </strong>", 
                           df$municipio, 
                           "<br><strong>Refugio: </strong>", 
                           df$refugio,
                           "<br><strong>Capacidad de personas: </strong>",
                           df$capacidad_personas,
                           "<br><strong>Latitud: </strong>",
                           round(df$latitud_val,2),
                           "<br><strong>Longitud: </strong>",
                           round(df$longitud_val,2),
                           "<br><strong>Altitud: </strong>",
                           round(df$altitud,2)
                           )
    
  output$mymap <-renderLeaflet({
    leaflet(data = map) %>%
    addTiles() %>%
    addPolygons(
      fillOpacity = 0.3, 
      smoothFactor = 0.5,
      color = "#BDBDC3", 
      weight = 5) %>%
    addCircleMarkers(-df$longitud_val,df$latitud_val,
                     radius=3,
                     color=pal(log(df$capacidad_personas)),
                     popup=refugios_popup) %>%
      addMeasure()
  })
  
  observe({
    leafletProxy('mymap') %>%
      clearMarkers() %>%
      addCircleMarkers(-df$longitud_val,df$latitud_val,
                       radius=3,
                       color=pal(log(df$capacidad_personas)),
                       popup=refugios_popup) %>%
      addMarkers(-lon_point(),lat_point(),popup="Estás aquí")
    
    
  })
  
 
  
}

shinyApp(ui = ui, server = server)

