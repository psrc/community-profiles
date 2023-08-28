# Display Tenure metric

rdi_tenure_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(title = "Tenure",
           tabsetPanel(
             id = ns('tab_tenure'),
             # renter ----
             tabPanel(title = 'Renter',
                      value = 'renter',
                      div(style = "padding-top: 1rem;",
                          fluidRow(
                            column(6,
                                   echarts4rOutput(ns('r_plot'))),
                            column(6,
                                   leafletOutput(ns('r_map'))
                            )
                          )# end fluidrow
                      ) # end div
                      , 
                      fluidRow(
                        column(width = 12,
                               uiOutput(ns('r_tableui'))
                        )
                        
                      ) # end fluidRow
             ), # end tabPanel Renter 
             tabPanel(title = 'Owner',
                      value = 'owner',
                      div(style = "padding-top: 1rem;",
                      fluidRow(
                      column(6,
                             echarts4rOutput(ns('o_plot'))),
                      column(6,
                             leafletOutput(ns('o_map')))
                      ), # end FluidRow
                      fluidRow(
                        column(width = 12,
                               uiOutput(ns('o_tableui'))
                        )
                        )
                      ) # end fluidRow
             ) # end tabPanel
           ) # end tabsetPanel
  )# end tabpanel
}

rdi_tenure_server <- function(id, shape, place) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    vals <- reactiveValues(source = 'Sources: US HUD, 2015-2019 Comprehensive Housing Affordability Strategy (CHAS) Table 9',
                           id_cols = c('chas_year', 'geography_name', 'description'))
    
    output$r_tableui <- renderUI({
      div(
        withSpinner(
          DTOutput(ns("r_table")),
          type = 5,
          color = psrc_colors$pgnobgy_10[sample.int(10, 1)]
        ),
        style = 'margin-top: 1rem'
      )
      
    })
    
    output$o_tableui <- renderUI({
      div(
        withSpinner(
          DTOutput(ns("o_table")),
          type = 5,
          color = psrc_colors$pgnobgy_10[sample.int(10, 1)]
        ),
        style = 'margin-top: 1rem'
      )
      
    })
    
    data <- reactive({
      # pull (currently from SQLite) semi-prepped CHAS

      df <- create_tenure_table(juris = 'place') %>%
        filter(geography_name == place())

      df_region <- create_tenure_table(juris = 'region')

      return(list(place = df, region = df_region))
    })
    
    table_data <- reactive({
      # data in wide for display in table
      
      region <- data()$region %>%
        select(-chas_year, -geography_name) %>%
        rename_with(~paste0(.x, '_region'))

      d <- left_join(data()$place, region, by = c('description' = 'description_region'))
      
      rdf <- d %>% 
        select(vals$id_cols, starts_with('all'), starts_with('renter'))
      odf <- d %>% 
        select(vals$id_cols, starts_with('all'), starts_with('owner'))
      
      return(list(r = rdf, o = odf))
    })
    
    plot_data <- reactive({
      # data in long form for plotting

      df <- bind_rows(data()) %>% 
        select(chas_year, geography_name, description, ends_with('share'))%>%
        pivot_longer(cols = ends_with('Share'),
                     names_to = 'type',
                     values_to = 'value')
      
        geog <- c(place(), 'Region')
    
        desc_rev <- rev(unique(df$description))

        df %>%
          mutate(type_desc = case_when(type == 'rental_share' ~ 'Rental Households',
                                       type == 'owner_share' ~ 'Owner Households')) %>%
          mutate(description = factor(description, levels = desc_rev)) %>%
          arrange(description)
    })
    
    place_name <- reactive({unique(data()$place$geography_name)})
    
    container <- reactive({
      # custom container for DT
      
      ifelse(input$tab_tenure == 'renter', tenure <- 'Renter', tenure <- 'Owner')

      htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(rowspan = 2, 'Race/Ethnicity of Householder'),
            th(class = 'dt-center', colspan = 3, place_name()),
            th(class = 'dt-center', colspan = 3, 'Region')
          ),
          tr(
            lapply(rep(c(paste(tenure, "Households"), "All Households", 'Share'), 2), th)
          )
        )
      ))
      
    })
    
    output$r_table <- renderDT({
      # table display
      
      d <- table_data()$r %>% 
        select(-chas_year, -geography_name) %>% 
        select(description, renter_occupied, all_units, renter_share, renter_occupied_region, all_units_region, renter_share_region)
      
      datatable(d,
                container = container(),
                rownames = FALSE,
                options = list(dom = 'tipr',
                               columnDefs = list(list(className = 'dt-center', targets = 1:6))),
                caption = htmltools::tags$caption(
                  style = 'caption-side: bottom; text-align: right;',
                  htmltools::em(vals$source)
                )) %>%
        formatPercentage(str_subset(colnames(d), ".*share(.)*$"), 1)
    })
    
    output$o_table <- renderDT({
      # table display
      
      d <- table_data()$o %>% 
        select(-chas_year, -geography_name) %>% 
        select(description, owner_occupied, all_units, owner_share, owner_occupied_region, all_units_region, owner_share_region)
      
      datatable(d,
                container = container(),
                rownames = FALSE,
                options = list(dom = 'tipr',
                               columnDefs = list(list(className = 'dt-center', targets = 1:6))),
                caption = htmltools::tags$caption(
                  style = 'caption-side: bottom; text-align: right;',
                  htmltools::em(vals$source)
                )) %>%
        formatPercentage(str_subset(colnames(d), ".*share(.)*$"), 1)
    })
    
    output$r_plot <- renderEcharts4r({
      
      echart_rdi(data = plot_data(),
                 filter_type = "renter_share",
                 desc_col = description,
                 str_wrap_num = 20,
                 group = geography_name,
                 x = 'description',
                 y = 'value',
                 ymax = 1,
                 title = 'Renter Households',
                 egrid_left = "20%")|>
          e_legend(bottom=0) |>
          e_group("grp")
    })
    
    output$o_plot <- renderEcharts4r({
      echart_rdi(data = plot_data(),
                 filter_type = "owner_share",
                 desc_col = description,
                 str_wrap_num = 20,
                 group = geography_name,
                 x = 'description',
                 y = 'value',
                 ymax = 1,
                 title = 'Owner Households',
                 egrid_left = "20%")|>
          e_legend(bottom=0) |>
          e_toolbox_feature("dataView") |>
          e_toolbox_feature("saveAsImage") |>
          e_group("grp") |>
          e_connect_group("grp")
    })
    
    map_data <- reactive({
      s <- shape %>% filter(geog_name == place())
    })
    
    output$r_map <- renderLeaflet({
      shp <- tract.shape %>%
        filter(census_year == 2010)

      d <- create_tenure_tract_table()

      y <- create_tenure_tract_map(table = d,
                                   tenure_type = 'Renter',
                                   shape_tract = shp,
                                   shape_place = map_data())
    })
    
    output$o_map <- renderLeaflet({
      shp <- tract.shape %>%
        filter(census_year == 2010)
      
      d <- create_tenure_tract_table()
      
      y <- create_tenure_tract_map(table = d,
                                   shape_tract = shp,
                                   shape_place = map_data())
    })
    
    
  }) # end moduleServer
  
}