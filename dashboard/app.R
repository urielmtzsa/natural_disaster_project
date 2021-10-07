
library(shiny)

ui <- fluidPage(
  titlePanel("REFUGIOS NAYARIT"),
  fluidRow(
    column(3,
           wellPanel(
             numericInput("Latitud", "Latitud", 21.49,20.5,23,0.1),
             numericInput("Longitud", "Longitud", 104.89,103.5,106,0.1)
           ),
           wellPanel(
             h4("Refugio:"),
             verbatimTextOutput("nombre"),
             verbatimTextOutput("verb")
           )
    ),
    column(8,
           leafletOutput("mymap"
           )))
             
  


)




server <- function(input, output,session){
  
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
                     popup=refugios_popup) 
  })
  
  
  
  observeEvent(input$mymap_click, {
    click <- input$mymap_click
    click_lat <- click$lat
    click_long <- click$lng
    
    updateNumericInput(session, "Latitud", "Latitud", click_lat,20.5,23,0.1)
    updateNumericInput(session, "Longitud", "Longitud", -click_long,103.5,106,0.1)
    
    
  })
  

  observe({
    
    x1<-df %>% select(longitud_val,latitud_val) %>% mutate(longitud_val=-longitud_val)
    x2<-data_frame(longitud_val=c(-lon_point()),latitud_val=c(lat_point()))
    x1<-as.data.frame(cdist(x1,x2,metric="manhattan"))
    names(x1)<-c("distance")
    x1<-cbind(df,x1) 
    x1<-x1  %>% top_n(-10,wt=distance) %>%  select(no,longitud_val,latitud_val) %>% 
      mutate(longitud_val=-longitud_val) 
    
    x2<-data_frame(no=c("Point"),longitud_val=c(-lon_point()),latitud_val=c(lat_point()))
    x1 <- osrmTable(src=x1,dst=x2,measure = c('duration', 'distance'),osrm.profile = "car")
    x1_distance<-as.data.frame(x1$distances)
    x1_duration<-as.data.frame(x1$durations)
    x1_distance$no<-rownames(x1_distance)
    x1_duration$no<-rownames(x1_duration)
    
    x1<-x1_distance %>% top_n(-5,wt=Point)
    x1<-merge(x1,x1_duration,by="no",suffixes=c("_distance","_duration"),all.x=TRUE,all.y=FALSE)
    x1<-merge(x1,df,by="no",all.x=TRUE,all.y=FALSE)
    x1<-x1 %>% arrange(Point_distance) 
    x1_p<-x1 %>%  select(no,longitud_val,latitud_val) %>% 
      mutate(longitud_val=-longitud_val) 
    
    ruta <- osrmRoute(src = x1_p[1,], dst = x2[1,], overview = "full", returnclass="sp")
    
    leafletProxy('mymap') %>%
      clearMarkers() %>%
      clearShapes() %>%
      addCircleMarkers(-df$longitud_val,df$latitud_val,
                       radius=3,
                       color=pal(log(df$capacidad_personas)),
                       popup=refugios_popup) %>%
    addAwesomeMarkers(lng=-lon_point(),lat=lat_point(),popup="<strong>¡Estás aquí! </strong>",
                      icon=awesomeIcons(markerColor = "darkblue")) %>%
    #addAwesomeMarkers(lng=x1_p[1,2],lat=x1_p[1,3],popup="Opción 1",
    #                    icon=awesomeIcons(icon = 'ios-close',markerColor = "green")) %>%
    #  addAwesomeMarkers(lng=x1_p[2,2],lat=x1_p[2,3],popup="Opción 1",
    #                    icon=awesomeIcons(icon = 'ios-close',markerColor = "gray")) %>%
    #  addAwesomeMarkers(lng=x1_p[3,2],lat=x1_p[3,3],popup="Opción 2",
    #                    icon=awesomeIcons(icon = 'ios-close',markerColor = "gray")) %>%
    #  addAwesomeMarkers(lng=x1_p[4,2],lat=x1_p[4,3],popup="Opción 3",
    #                    icon=awesomeIcons(icon = 'ios-close',markerColor = "gray")) %>%
    addPolylines(data=ruta,color = "darkblue") 
    
    
    
    
    
    output$nombre <- renderText({x1[1,"refugio"]  })
    output$verb <- renderText({ lat_point() })
    
    
  })
  
  
  
  
  
 
  
}

shinyApp(ui = ui, server = server)

