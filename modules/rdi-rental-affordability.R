# Display metric

rdi_rentaff_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(title = "Rental Affordability",
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

rdi_rentaff_server <- function(id, shape, place) {
  
  moduleServer(id, function(input, output, session) { 
    # ns <- session$ns
    
    data <- reactive({
      # pull (currently from Elmer) semi-prepped CHAS
      
      df <- create_rental_affordability_table() %>% 
        filter(geography_name == place())
    })
    
    
    
    output$table <- renderDT({
      
      place_name <- reactive(unique(data()$geography_name))
      
      d <- data() %>% 
        select(description, renter_hh_income, rental_units, ends_with('share'))
      
      sketch <-  htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(rowspan = 2, 'Affordability'),
            rep(list(th(class = 'dt-center', colspan = 2, place_name())), 2)
          ),
          tr(
            # rep(lapply(c('Household', 'Rental Units'), function(x) th(class = 'dt-center', x)), 2)
            lapply(rep(c("Households", "Rental Units"), 2), th)
          )
        )
      ))

      datatable(d,
                container = sketch,
                rownames = FALSE,
                options = list(columnDefs = list(list(className = 'dt-center', targets = 1:4)))) %>% 
        formatPercentage(str_subset(colnames(d), ".*share$"), 1)
    })
    
    
    output$plot <- renderPlot({
      ggplot(mtcars) +
        geom_point(aes(mpg, cyl))
    })
    
    map_data <- reactive({
      s <- shape %>% filter(geog_name == place())
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