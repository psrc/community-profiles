# Display Tenure metric

rdi_tenure_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(title = "Tenure",
           tabsetPanel(
             # renter ----
             tabPanel(title = 'Renter',
                      div(style = "padding-top: 1rem;",
                          fluidRow(
                            column(4,
                                   echarts4rOutput(ns('r_plot'))),
                            column(4,
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
                      fluidRow(
                      column(4,
                             echarts4rOutput(ns('o_plot'))),
                      column(4,
                             leafletOutput(ns('o_map')))
                      ), # end FluidRow
                      fluidRow(
                        column(width = 12,
                               uiOutput(ns('o_tableui'))
                        )
                      ) # end fluidRow
             ) # end tabPanel
           ) # end tabsetPanel
  )# end tabpanel
}

rdi_tenure_server <- function(id, shape, place) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
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
      
      # region <- data()$region %>%
      #   select(description, renter_occupied, owner_occupied, ends_with('share')) %>%
      #   rename_with(~paste0(.x, '_reg'))
      # 
      # d <- left_join(data()$place, region, by = c('description' = 'description_reg')) %>%
      #   select(description, renter_occupied, owner_occupied, ends_with('share'), ends_with('reg'))
      id_cols <- c('chas_year', 'geography_name', 'description')
      
      region <- data()$region %>%
        select(-chas_year, -geography_name) %>%
        rename_with(~paste0(.x, '_region'))

      d <- left_join(data()$place, region, by = c('description' = 'description_region'))
      
      rdf <- d %>% 
        select(id_cols, starts_with('all'), starts_with('renter'))
      odf <- d %>% 
        select(id_cols, starts_with('all'), starts_with('owner'))
      
      return(list(r = rdf, o = odf))
    })
    
    # plot_data <- reactive({
    #   # data in long form for plotting
    #   
    #   df <- bind_rows(data()) %>%
    #     filter(description != 'All') %>%
    #     select(chas_year, geography_name, description, ends_with('share')) %>%
    #     pivot_longer(cols = ends_with('Share'),
    #                  names_to = 'type',
    #                  values_to = 'value')
    # })
    
    # plot_clean_data <- reactive({
    #   # munge long form data for visual
    #   
    #   geog <- c(place(), 'Region')
    # 
    #   desc_rev <- rev(unique(plot_data()$description))
    #   
    #   plot_data() %>%
    #     mutate(type_desc = case_when(type == 'rental_share' ~ 'Rental Households',
    #                                  type == 'owner_share' ~ 'Owner Households')) %>%
    #     mutate(description = factor(description, levels = desc_rev)) %>%
    #     arrange(description)
    # })
    
    place_name <- reactive({unique(data()$place$geography_name)})
    
    container <- reactive({
      # custom container for DT
      
      htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(rowspan = 2, 'Race/Ethnicity of Householder'),
            th(class = 'dt-center', colspan = 3, place_name()),
            th(class = 'dt-center', colspan = 3, 'Region')
          ),
          tr(
            lapply(rep(c("Households", "All Households", 'Share'), 2), th)
          )
        )
      ))
    })
    
    output$r_table <- renderDT({
      # table display
      
      source <- 'Sources: US HUD, 2015-2019 Comprehensive Housing Affordability Strategy (CHAS) Table 9'
      
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
                  htmltools::em(source)
                )) %>%
        formatPercentage(str_subset(colnames(d), ".*share(.)*$"), 1)
    })
    
    output$o_table <- renderDT({
      # table display
      
      source <- 'Sources: US HUD, 2015-2019 Comprehensive Housing Affordability Strategy (CHAS) Table 9'
      
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
                  htmltools::em(source)
                )) %>%
        formatPercentage(str_subset(colnames(d), ".*share(.)*$"), 1)
    })
    
    # output$table <- renderDT({
    #   # table display
    #   
    #   source <- 'Sources: US HUD, 2015-2019 Comprehensive Housing Affordability Strategy (CHAS) Table 9'
    #   
    #   datatable(table_data(),
    #             container = container(),
    #             rownames = FALSE,
    #             options = list(dom = 'tipr',
    #                            columnDefs = list(list(className = 'dt-center', targets = 1:8))),
    #             caption = htmltools::tags$caption(
    #               style = 'caption-side: bottom; text-align: right;',
    #               htmltools::em(source)
    #             )) %>%
    #     formatPercentage(str_subset(colnames(table_data()), ".*share(.)*$"), 1)
    # })
    
    # output$plot01 <- renderEcharts4r({
    #   echart_rdi(data = plot_clean_data(),
    #              filter_type = "renter_share",
    #              desc_col = description, 
    #              str_wrap_num = 15,
    #              group = geography_name,
    #              x = 'description',
    #              y = 'value',
    #              title = 'Renter Households',
    #              egrid_left = "30%")|>
    #       e_legend(bottom=0) |>
    #       e_group("grp")
    # })
    
    # output$plot02 <- renderEcharts4r({
    #   echart_rdi(data = plot_clean_data(),
    #              filter_type = "owner_share",
    #              desc_col = description, 
    #              str_wrap_num = 15,
    #              group = geography_name,
    #              x = 'description',
    #              y = 'value',
    #              title = 'Owner Households',
    #              egrid_left = "30%")|>
    #       e_legend(show=FALSE) |>
    #       e_toolbox_feature("dataView") |>
    #       e_toolbox_feature("saveAsImage") |>
    #       e_group("grp") |>
    #       e_connect_group("grp")
    # })
    
    map_data <- reactive({
      s <- shape %>% filter(geog_name == place())
    })
    
    # output$map <- renderLeaflet({
    #   shp <- tract.shape %>% 
    #     filter(census_year == 2010)
    # 
    #   d <- create_tenure_tract_table()
    # 
    #   y <- create_tenure_tract_map(table = d, 
    #                                shape_tract = shp, 
    #                                shape_place = map_data())
    # })
    
    
  }) # end moduleServer
  
}