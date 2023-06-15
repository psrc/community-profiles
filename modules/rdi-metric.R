# Display metric

rdi_metric_ui <- function(id, metric) {
  ns <- NS(id)
  
  tabPanel(title = metric,
           fluidRow(
             column(6,
                    plotOutput(ns('plot'))),
             column(6,
                    leafletOutput(ns('map'))
                    )
           ), # end fluidrow
           fluidRow(
             column(width = 12,
                    DTOutput(ns("table"))
             )
           ) # end fluidRow
  ) # end tabpanel
  
}

rdi_metric_server <- function(id, shape, place) {
  
  moduleServer(id, function(input, output, session) { 
    # ns <- session$ns
    
    output$table <- renderDT(datatable(mtcars))
    
    output$plot <- renderPlot({
      ggplot(mtcars) +
        geom_point(aes(mpg, cyl))
    })
    
    map_data <- reactive({
      shape %>% filter(geog_name == place())
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