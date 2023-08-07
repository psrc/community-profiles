# Display Cost Burden metric

rdi_income_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(title = "Income",
           tabsetPanel(
             # renter ----
             tabPanel(title = 'Renter',
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
             
             # owner ----
             
             tabPanel(title = 'Owner',
                      div(style = "padding-top: 1rem;",
                          fluidRow(
                            column(6,
                                   echarts4rOutput(ns('o_plot'))),
                            column(6,
                                   leafletOutput(ns('o_map'))
                            )
                          )# end fluidrow
                      ) # end div
                      ,
                      fluidRow(
                        tabsetPanel(type = 'pills',
                                    tabPanel(title = 'Estimate',
                                             column(width = 12,
                                                    uiOutput(ns('o_e_tableui')))),
                                    tabPanel(title = 'Share',
                                             column(width = 12,
                                                    uiOutput(ns('o_s_tableui'))))
                        ) # end tabsetPanel
                      ) # end fluidRow
             ) # end owner tabPanel
             
           ) # end tabsetPanel
  ) # end tabpanel
  
}

rdi_income_server <- function(id, shape, place) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    vals <- reactiveValues(source = 'Sources: US HUD, 2015-2019 Comprehensive Housing Affordability Strategy (CHAS) Table 1',
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
    
    output$o_e_tableui <- renderUI({
      div(
        withSpinner(
          DTOutput(ns("o_e_table")),
          type = 5,
          color = psrc_colors$pgnobgy_10[sample.int(10, 1)]
        ),
        style = 'margin-top: 1rem'
      )
    })
    
    output$o_s_tableui <- renderUI({
      div(
        withSpinner(
          DTOutput(ns("o_s_table")),
          type = 5,
          color = psrc_colors$pgnobgy_10[sample.int(10, 1)]
        ),
        style = 'margin-top: 1rem'
      )
    })
    
    data <- reactive({
      # pull (currently from SQLite) semi-prepped CHAS
      
      dfs <- create_income_table(juris = 'place') 
      
      rdfs <- map(dfs, ~filter(.x, geography_name == place() & tenure == 'Renter occupied'))
      odfs <- map(dfs, ~filter(.x, geography_name == place() & tenure == 'Owner occupied'))
      
      return(list(r = rdfs, o = odfs))
    })
    
    plot_data <- reactive({
      # data (shares) for renter and owner in long form for plotting
      
      dfs <- map(data(), ~.x[['s']])

      pivot_table_longer <- function(table) {
      table %>%
        filter(!(income_grp %in% c(str_subset(income_grp, "All")))) %>%
        pivot_longer(cols = setdiff(colnames(table), c(vals$exc_cols, 'income_grp')),
                     names_to = 'race_ethnicity',
                     values_to = 'value')
      }

      dfs_share <- map(dfs, ~pivot_table_longer(.x))
    })
    
    plot_clean_data <- reactive({
      # munge long form data (shares) for renter and owner for plotting
      
      filter_set_levels <- function(table) {
        df <- table %>%
          filter(!race_ethnicity %in% c('POC', 'Total'))

        desc_rev <- rev(unique(df$race_ethnicity))

        df %>%
          mutate(race_ethnicity = factor(race_ethnicity, levels = desc_rev)) %>%
          arrange(race_ethnicity)
      }
      
      dfs <- map(plot_data(), ~filter_set_levels(.x))
    })
    
    place_name <- reactive({unique(data()$r$e$geography_name)})
    
    map_data <- reactive({
      s <- shape %>% filter(geog_name == place())
    })
    
    container <- reactive({
      # custom container for DT
      
      selcols <- colnames(data()$r$e)[which(!(colnames(data()$r$e) %in% c('chas_year', 'geography_name', 'tenure', 'income_grp')))]
      selcols <- str_replace_all(selcols, "POC", "People of Color (POC)")
      
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
    
    # Renter ----
    
    output$r_e_table <- renderDT({
      # Renter Estimate table display
      
      exc_cols <- vals$exc_cols
      d <- data()$r$e[,!..exc_cols]

      create_dt_income(table = d, container = container(), source = vals$source)
    })
    
    output$r_s_table <- renderDT({
      # Renter Share table display
      
      exc_cols <- vals$exc_cols
      d <- data()$r$s[,!..exc_cols]

      create_dt_income(table = d, container = container(), source = vals$source) %>%
        formatPercentage(2:9, 1)
    })
    
    output$r_plot <- renderEcharts4r({
      
      echart_rdi(data = plot_clean_data()$r,
                 desc_col = race_ethnicity,
                 str_wrap_num = 15,
                 group = income_grp,
                 x = 'race_ethnicity',
                 y = 'value',
                 ymax = 1,
                 stack = 'grp',
                 title = 'Renter Households',
                 egrid_left = "15%")|>
        e_x_axis(formatter = e_axis_formatter("percent", digits = 0))|>
        e_legend(bottom=0) |>
        e_toolbox_feature("dataView") |>
        e_toolbox_feature("saveAsImage")
    })
    
    # Owner ----
    
    output$o_e_table <- renderDT({
      # Owner Estimate table display
      
      exc_cols <- vals$exc_cols
      d <- data()$o$e[,!..exc_cols]

      create_dt_income(table = d, container = container(), source = vals$source)
    })
    
    output$o_s_table <- renderDT({
      # Owner Share table display

      exc_cols <- vals$exc_cols
      d <- data()$o$s[,!..exc_cols]

      create_dt_income(table = d, container = container(), source = vals$source) %>%
        formatPercentage(2:9, 1)
    })
    
    output$o_plot <- renderEcharts4r({
      
      echart_rdi(data = plot_clean_data()$o,
                 desc_col = race_ethnicity,
                 str_wrap_num = 15,
                 group = income_grp,
                 x = 'race_ethnicity',
                 y = 'value',
                 ymax = 1,
                 stack = 'grp',
                 title = 'Owner Households',
                 egrid_left = "15%")|>
        e_x_axis(formatter = e_axis_formatter("percent", digits = 0))|>
        e_legend(bottom=0) |>
        e_toolbox_feature("dataView") |>
        e_toolbox_feature("saveAsImage")
      
    })
    
    output$r_map <- renderLeaflet({
      shp <- tract.shape %>%
        filter(census_year == 2010)

      d <- create_income_tract_table()

      create_income_tract_map(table = d, tenure_type = "Renter", shape_tract = shp, shape_place = map_data())
    })
    
    output$o_map <- renderLeaflet({
      shp <- tract.shape %>%
        filter(census_year == 2010)

      d <- create_income_tract_table()

      create_income_tract_map(table = d, tenure_type = "Owner", shape_tract = shp, shape_place = map_data())
    })
    
    
  }) # end moduleServer
  
}