# Display Displacement Risk 

rdi_disp_risk_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(title = "Displacement Risk",
           div(style = "padding-top: 1rem;",
               fluidRow(
                 column(width = 6, 
                        p("Hold for description")
                 ),
                 column(width = 6, 
                        leafletOutput(ns("map"))
                 ),
                 style = "margin-top: 1rem;"
               ), # end fluidRow 
               fluidRow(
                 column(width = 12, 
                        DTOutput(ns("table")
                        )
                 )
               ) # end fluidRow
           ) # end div
           
  )# end tabpanel
}

rdi_disp_risk_server <- function(id, shape, place, disp_risk_shape) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    vals <- reactiveValues(source = 'Sources: ',
                           id_cols = c('chas_year', 'geography_name', 'description'))
    
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
      
      
      
      
    })
    
    table_data <- reactive({
      # pull (currently from SQLite)
      # data in wide for display in table
      
      rename_cols <- function(df) {
        df %>% 
          rename_with(~paste0(paste0(unique(df$planning_geog), "_"), .x, recycle0 = TRUE), c('lower', 'moderate', 'higher', 'All')) %>% 
          select(-planning_geog)
      }
      
      df <- read.dt.disprisk(type = 'table', "acs5_2022_B03002_tract_juris_split_summary_shares")
      
      juris <- df %>% 
        filter(planning_geog == place()) 
      j <- rename_cols(juris)
      
      region <- df %>% 
        filter(planning_geog == 'Region')
      r <- rename_cols(region)
      
      t <- left_join(j, r, by = 'race_ethnicity_label')
    })
    
    plot_data <- reactive({
      # data in long form for plotting
      
      # data()
      
    })
    
    place_name <- reactive({
      place()
    })
    
    container <- reactive({
      # custom container for DT
      
       selcols = c('Lower', 'Moderate', 'Higher', "All")
        
        htmltools::withTags(table(
          class = 'display',
          thead(
            tr(
              th(rowspan = 2, 'Race/Ethnicity'),
              th(class = 'dt-center', colspan = 4, place_name()),
              th(class = 'dt-center', colspan = 4, 'Region')
            ),
            tr(
              lapply(rep(selcols, 2), th)
            )
          )
        ))
      
    })
    
    output$table <- renderDT({
      # table display
      t <- table_data()

      selcols <- str_subset(colnames(t), ".*er")
      selcols2 <- str_subset(colnames(t), ".*All")
      
      source <- "Sources: American Community Survey (ACS) 2018-2022 Table B03002, Puget Sound Regional Council (PSRC)"
      
      datatable(table_data(),
                container = container(),
                rownames = FALSE,
                options = list(dom = 'tipr',
                               columnDefs = list(list(className = 'dt-center', targets = 1:8))),
                caption = htmltools::tags$caption(
                  style = 'caption-side: bottom; text-align: right;',
                  htmltools::em(source))) %>% 
        formatPercentage(selcols, 2) %>% 
        formatRound(selcols2, 0)
      
      # https://stackoverflow.com/questions/40224925/r-shiny-mouseover-to-all-table-cells/40634033#40634033
      
      # can hover and have tooltip? yes
      # can hide columns but still reference them in tooltip? yes
      # how to have tooltip for each cell in table?
      
      # td (table html ref)
      # eq(3) = third column
      # nRow = each row
      #$('td', nRow) all cells for each row
      # .attr('data-title', full_text) apply data-title attribute css to text
      # datatable(palmerpenguins::penguins,
      #           options = list(columnDefs = list(list(visible = FALSE, targets = c(5, 6, 7))),
      #                          rowCallback = JS(
      #                            "function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {",
      #                            "var full_text = aData[0] + ','+ aData[1] + ',' + aData[2] + ','+ aData[7];",
      #                            "$('td', nRow).attr('data-title', full_text);",
      #                            "}")
      #           )
      # )
      
    })
    
   
    
    
    map_data <- reactive({
      s <- shape %>% filter(geog_name == place())
    })
    
    output$map <- renderLeaflet({
      
      # d is tract table with data
      # shape_tract can just be displacement risk shape
      
      # s <- shp %>%
      #   left_join(d, by = c('geoid' = 'tract_geoid'))
      
      m <- create_displacement_risk_map(shape_tract = disp_risk_shape,
                                        shape_place = map_data(), 
                                        title = paste('Displacement Risk'))
      

      
      
      
      
      
      
      # # shp <- tract.shape %>%
      # #   filter(census_year == 2010)
      # # https://stackoverflow.com/questions/48696395/leaflet-mixing-continuous-and-discrete-colors
      # disprisk.shape <- disprisk.shape %>% 
      #   mutate(risk_level_name = factor(risk_level_name, levels = c("lower", "moderate", "higher")))
      # dispal <- colorFactor(palette = c("#d8b365", "#f5f5f5", "#5ab4ac"), levels = unique(disp_risk_shape$risk_level_name), na.color = "grey")
      # 
      # leaflet(disp_risk_shape) %>% 
      #   addProviderTiles(providers$CartoDB.Positron) %>% 
      #   addPolygons(fillOpacity = 1,
      #               weight = 1,
      #               fillColor = ~dispal(risk_level_name),
      #               group = "Census Tracts") #%>%
      #   # addPolygons(data = map_data(),
      #   #             fill = FALSE,
      #   #             weight = 3,
      #   #             opacity = 1.0,
      #   #             color = "#91268F",
      #   #             dashArray = "4",
      #   #             group = "Place Boundary")

    })
    
    
  }) # end moduleServer
  
}