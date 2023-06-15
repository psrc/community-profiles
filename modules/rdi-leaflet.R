# Module to display leaflet map

leaflet_ui <- function(id) {
  ns <- NS(id)
  
  column(6,
         leafletOutput(ns('map'))
         )
}

leaflet_server <- function(id, shape, place) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    map_data <- reactive({
      s <- shape %>% filter(geog_name == place)
      })
    
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = map_data(),
                    fillColor = "#76787A",
                    weight = 4,
                    opacity = 1.0,
                    color = "#91268F",
                    dashArray = "4",
                    fillOpacity = 0.0)
    })
    
  }) # end moduleServer
  
}