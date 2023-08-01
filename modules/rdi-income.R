# Display Income metric

rdi_income_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(title = "Income",
           div(style = "padding-top: 1rem;",
               fluidRow(
                 column(4,
                        echarts4rOutput(ns('plot01'))),
                 column(4,
                        echarts4rOutput(ns('plot02'))),
                 
                 column(4,
                        leafletOutput(ns('map'))
                 )
               )# end fluidrow
           ) # end div
           , 
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
      # pull (currently from SQLite) semi-prepped CHAS
      
   
    })
    
    table_data <- reactive({
      # data in wide for display in table
      
     
    })
    
    plot_data <- reactive({
      # data in long form for plotting
      
     
    })
    
    plot_clean_data <- reactive({
      # munge long form data for visual
      
      
    })
    
    place_name <- reactive({unique(data()$place$geography_name)})
    
    container <- reactive({
      # custom container for DT
      
      # selcols <- colnames(data()$r$e)[which(!(colnames(data()$r$e) %in% c('chas_year', 'geography_name', 'tenure', 'description')))]
      # selcols <- str_replace_all(selcols, "POC", "People of Color (POC)")
      
      htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(rowspan = 2, 'Income'),
            th(class = 'dt-center', colspan = 9, place_name())
          ),
          tr(
            lapply(selcols, th)
          )
        )
      ))
    })
    
    output$table <- renderDT({
      # table display
      
      source <- 'Sources: US HUD, 2015-2019 Comprehensive Housing Affordability Strategy (CHAS) Tables 1'
      
      
    })
    
    output$plot01 <- renderEcharts4r({
    
    })
    
    output$plot02 <- renderEcharts4r({
   
    })
    
    map_data <- reactive({
      s <- shape %>% filter(geog_name == place())
    })
    
    output$map <- renderLeaflet({
      shp <- tract.shape %>% 
        filter(census_year == 2010)
      
      # d <- create_rental_affordability_tract_table()
      # 
      # s <- shp %>%
      #   left_join(d, by = c('geoid' = 'tract_geoid'))
      # 
      # m <- create_chas_tract_map(shape_tract = s,
      #                            shape_place = map_data(), 
      #                            title = paste('Census Tracts of Rental Units', 'Less than 80% AMI', sep = "<br>"))
    })
    
    
  }) # end moduleServer
  
}