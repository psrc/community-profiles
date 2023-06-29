# Display Rental Affordability metric

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
                      uiOutput(ns('tableui'))
                      )
             
           ) # end fluidRow
  ) # end tabpanel
  
}

rdi_rentaff_server <- function(id, shape, place) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    output$tableui <- renderUI({
      div(
        withSpinner(
          DTOutput(ns("table")),
          type = 5,
          color = psrc_colors$pgnobgy_10[sample.int(10, 1)]
        ),
        style = 'margin-top: 1rem'
      )
      
    })
    
    data <- reactive({
      # pull (currently from Elmer) semi-prepped CHAS

      df <- create_rental_affordability_table(juris = 'place') %>% 
        filter(geography_name == place())

      df_region <- create_rental_affordability_table(juris = 'county') %>% 
        select(description, renter_hh_income, rental_units, ends_with('share')) %>% 
        rename_with(~paste0(.x, '_reg'))
      
      d <- left_join(df, df_region, by = c('description' = 'description_reg'))
      
    })
    
    container <- reactive({
      # custom container for DT
      
      place_name <- reactive(unique(data()$geography_name))
      
      htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(rowspan = 2, 'Affordability'),
            th(class = 'dt-center', colspan = 4, place_name()),
            th(class = 'dt-center', colspan = 4, 'Region')
          ),
          tr(
            lapply(rep(c("Households", "Rental Units"), 4), th)
          )
        )
      ))
    })
    
    output$table <- renderDT({
      
      source <- 'Sources: US HUD, 2015-2019 Comprehensive Housing Affordability Strategy (CHAS) Tables 8, 14B, 15C'
     
      d <- data() %>% 
        select(description, renter_hh_income, rental_units, ends_with('share'), ends_with('reg'))

      datatable(d,
                container = container(),
                rownames = FALSE,
                options = list(columnDefs = list(list(className = 'dt-center', targets = 1:8))),
                caption = htmltools::tags$caption(
                  style = 'caption-side: bottom; text-align: right;',
                  htmltools::em(source)
                )) %>% 
        formatPercentage(str_subset(colnames(d), ".*share(.)*$"), 1)
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