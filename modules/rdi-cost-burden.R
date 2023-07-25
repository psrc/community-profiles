# Display Cost Burden metric

rdi_cost_burden_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(title = "Cost Burden",
           tabsetPanel(
             # renter ----
             tabPanel(title = 'Renter',
                      div(style = "padding-top: 1rem;",
                          fluidRow(
                            column(6,
                                   echarts4rOutput(ns('r_plot01'))),
                            # column(4,
                            #        echarts4rOutput(ns('r_plot02'))),
                            
                            column(6,
                                   leafletOutput(ns('r_map'))
                            )
                          )# end fluidrow
                      ) # end div
                      ,
                      fluidRow(
                        tabsetPanel(type = 'pills',
                          tabPanel(title = 'Estimate',
                                   column(width = 12,
                                          uiOutput(ns('r_e_tableui')))),
                          tabPanel(title = 'Share',
                                   column(width = 12,
                                          uiOutput(ns('r_s_tableui'))))
                        
                        )
                        
                      ) # end fluidRow
             ), # end Renter tabPanel
           tabPanel(title = 'Owner')
         
           ) # end tabsetPanel
  ) # end tabpanel
  
}

rdi_cost_burden_server <- function(id, shape, place) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    vals <- reactiveValues(source = 'Sources: US HUD, 2015-2019 Comprehensive Housing Affordability Strategy (CHAS) Table 9',
                           exc_cols = c('chas_year', 'geography_name', 'tenure'))
    
    output$r_e_tableui <- renderUI({
      div(
        withSpinner(
          DTOutput(ns("r_e_table")),
          type = 5,
          color = psrc_colors$pgnobgy_10[sample.int(10, 1)]
        ),
        style = 'margin-top: 1rem'
      )
    })
    
    output$r_s_tableui <- renderUI({
      div(
        withSpinner(
          DTOutput(ns("r_s_table")),
          type = 5,
          color = psrc_colors$pgnobgy_10[sample.int(10, 1)]
        ),
        style = 'margin-top: 1rem'
      )
    })

    data <- reactive({
      # pull (currently from SQLite) semi-prepped CHAS

      dfs <- create_cost_burden_table(juris = 'place') 

      rdfs <- map(dfs, ~filter(.x, geography_name == place() & tenure == 'Renter occupied'))
      odfs <- map(dfs, ~filter(.x, geography_name == place() & tenure == 'Owner occupied'))
      
      return(list(r = rdfs, o = odfs))
    })

    table_data <- reactive({
      # data in wide for display in table

      # region <- data()$region %>%
      #   select(description, renter_occupied, owner_occupied, ends_with('share')) %>%
      #   rename_with(~paste0(.x, '_reg'))
      #
      # d <- left_join(data()$place, region, by = c('description' = 'description_reg')) %>%
      #   select(description, renter_occupied, owner_occupied, ends_with('share'), ends_with('reg'))
    })

    # plot_data <- reactive({
    #   # data in long form for plotting
    #   
    #   # df <- bind_rows(data()) %>%
    #   #   filter(description != 'All') %>%
    #   #   select(chas_year, geography_name, description, ends_with('share')) %>%
    #   #   pivot_longer(cols = ends_with('Share'),
    #   #                names_to = 'type',
    #   #                values_to = 'value')
    # })
    # 
    # plot_clean_data <- reactive({
    #   # munge long form data for visual
    #   
    #   # geog <- c(place(), 'Region')
    #   # 
    #   # desc_rev <- rev(unique(plot_data()$description))
    #   # 
    #   # plot_data() %>%
    #   #   mutate(type_desc = case_when(type == 'rental_share' ~ 'Rental Households',
    #   #                                type == 'owner_share' ~ 'Owner Households')) %>%
    #   #   mutate(description = factor(description, levels = desc_rev)) %>%
    #   #   arrange(description)
    # })
    
    place_name <- reactive({unique(data()$r$e$geography_name)})
    
    container <- reactive({
      # custom container for DT
      
      selcols <- colnames(data()$r$e)[which(!(colnames(data()$r$e) %in% c('chas_year', 'geography_name', 'tenure', 'description')))]
      selcols <- str_replace_all(selcols, "POC", "People of Color (POC)")
      
      htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(rowspan = 2, 'Cost Burden'),
            th(class = 'dt-center', colspan = 9, place_name())
          ),
          tr(
            lapply(selcols, th)
          )
        )
      ))
    })
    
    output$r_e_table <- renderDT({
      # Renter Estimate table display
      
      exc_cols <- vals$exc_cols
      d <- data()$r$e[,!..exc_cols]
      
      create_dt_cost_burden(table = d, container = container(), source = vals$source)
    })
    
    output$r_s_table <- renderDT({
      # Renter Share table display

      exc_cols <- vals$exc_cols
      d <- data()$r$s[,!..exc_cols]
      
      create_dt_cost_burden(table = d, container = container(), source = vals$source) %>%
        formatPercentage(2:10, 1)
    })

    # output$plot01 <- renderEcharts4r({
    #   # echart_rdi(data = plot_clean_data(),
    #   #            filter_type = "renter_share",
    #   #            desc_col = description, 
    #   #            str_wrap_num = 15,
    #   #            group = geography_name,
    #   #            x = 'description',
    #   #            y = 'value',
    #   #            title = 'Renter Households',
    #   #            egrid_left = "30%")|>
    #   #   e_legend(bottom=0) |>
    #   #   e_group("grp")
    # })
    # 
    # output$plot02 <- renderEcharts4r({
    #   # echart_rdi(data = plot_clean_data(),
    #   #            filter_type = "owner_share",
    #   #            desc_col = description, 
    #   #            str_wrap_num = 15,
    #   #            group = geography_name,
    #   #            x = 'description',
    #   #            y = 'value',
    #   #            title = 'Owner Households',
    #   #            egrid_left = "30%")|>
    #   #   e_legend(show=FALSE) |>
    #   #   e_toolbox_feature("dataView") |>
    #   #   e_toolbox_feature("saveAsImage") |>
    #   #   e_group("grp") |>
    #   #   e_connect_group("grp")
    # })
    # 
    # map_data <- reactive({
    #   s <- shape %>% filter(geog_name == place())
    # })
    # 
    # output$map <- renderLeaflet({
    #   # shp <- tract.shape %>% 
    #   #   filter(census_year == 2010)
    #   # 
    #   # d <- create_tenure_tract_table()
    #   # 
    #   # y <- create_tenure_tract_map(table = d, 
    #   #                              shape_tract = shp, 
    #   #                              shape_place = map_data())
    # })
    
    
  }) # end moduleServer
  
}