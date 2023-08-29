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
      
      ifelse(str_detect(place(), ".*County"), j <- 'county', j <- 'place') 
 
      p_dfs <- create_cost_burden_table(juris = j)
      r_dfs <- create_cost_burden_table(juris = 'region') 
      
      p_dfs <- map(p_dfs, ~filter(.x, geography_name == place()))
      
      rdfs <- odfs <- list()
      tables_list <- list(p = p_dfs, r = r_dfs)
      
      for(a_list in 1:length(tables_list)) {
        # extract only renter data
        
        d <- map(tables_list[[a_list]], ~filter(.x, tenure == 'Renter occupied'))
        if(names(tables_list[a_list]) == 'p') {
          names(d) <- paste0('p', names(d))
          rdfs[[a_list]] <- d
          
        } else {
          names(d) <- paste0('r', names(d))
          rdfs[[a_list]] <- d
        }
      }
      
      for(a_list in 1:length(tables_list)) {
        # extract only owner data
        
        d <- map(tables_list[[a_list]], ~filter(.x, tenure == 'Owner occupied'))
        if(names(tables_list[a_list]) == 'p') {
          names(d) <- paste0('p', names(d))
          odfs[[a_list]] <- d
          
        } else {
          names(d) <- paste0('r', names(d))
          odfs[[a_list]] <- d
        }
      }
      
      rdfs <- list_flatten(rdfs)
      odfs <- list_flatten(odfs)
     
      return(list(r = rdfs, o = odfs))
    })

    plot_data <- reactive({
      # data (shares) for renter and owner in long form for plotting

      # extract only share tables
      tables <- list()
      d <- data()
      for(i in 1:length(d)) {
        x <- rbind(pluck(d[[i]], 'ps'), pluck(d[[i]], 'rs'))
        tables[[i]] <- x
      }

      # pivot longer & filter
      tables <- map(tables, ~pivot_longer(.x, starts_with('Not')|contains('Cost'), names_to = 'cost_burden', values_to = 'share'))
      tables <- map(tables, ~filter(.x, cost_burden == "Total Cost-Burdened (>30%)"))

      race_levels <- rev(unique(tables[[1]]$race_ethnicity))
      tables <- map(tables, ~mutate(.x, race_ethnicity = factor(race_ethnicity, levels = race_levels)) %>% arrange(geography_name, race_ethnicity))
      
      return(list(r = tables[[1]], o = tables[[2]]))
    })

    place_name <- reactive({unique(data()$r$pe$geography_name)})
    
    map_data <- reactive({
      s <- shape %>% filter(geog_name == place())
    })
    
    container <- reactive({
      # custom container for DT
      
      selcols <- colnames(data()$r$pe)[which(!(colnames(data()$r$pe) %in% c('chas_year', 'geography_name', 'tenure', 'race_ethnicity')))]
      selcols <- c(selcols, "Total Cost-Burdened (>30%)", "All")
      
      htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(rowspan = 2, 'Race/Ethnicity'),
            th(class = 'dt-center', colspan = 6, place_name()),
            th(class = 'dt-center', colspan = 2, 'Region')
          ),
          tr(
            lapply(selcols, th)
          )
        )
      ))

    })
    
    prep_cost_burden_table <- function(place_table, regional_table) {

      dr <- regional_table %>% 
        select(race_ethnicity, `Total Cost-Burdened (>30%)`, All)
      colnames(dr) <- paste0(colnames(dr), '_region')  

      d <- left_join(place_table, dr, by = c('race_ethnicity' = 'race_ethnicity_region')) %>% 
        select(-chas_year, -geography_name, -tenure)
    }
    
    # Renter ----
    
    output$r_e_table <- renderDT({
      # Renter Estimate table display
      
      d <- prep_cost_burden_table(data()$r[['pe']], data()$r[['re']])

      create_dt_cost_burden(table = d, container = container(), source = vals$source)
    })
    
    output$r_s_table <- renderDT({
      # Renter Share table display
      
      d <- prep_cost_burden_table(data()$r[['ps']], data()$r[['rs']])
      
      create_dt_cost_burden(table = d, container = container(), source = vals$source) %>%
        formatPercentage(2:9, 1)
    })

    output$r_plot <- renderEcharts4r({

      echart_rdi(data = plot_data()$r,
                 desc_col = race_ethnicity,
                 str_wrap_num = 20,
                 group = geography_name,
                 x = 'race_ethnicity',
                 y = 'share',
                 ymax = 1,
                 title = 'Cost-Burdened Renter Households (>30%)',
                 egrid_left = "20%")|>
        e_x_axis(formatter = e_axis_formatter("percent", digits = 0))|>
        e_legend(bottom=0) |>
        e_toolbox_feature("dataView") |>
        e_toolbox_feature("saveAsImage")
    })

    # Owner ----
    
    output$o_e_table <- renderDT({
      # Owner Estimate table display

      d <- prep_cost_burden_table(data()$o[['pe']], data()$o[['re']])
      
      create_dt_cost_burden(table = d, container = container(), source = vals$source)
    })

    output$o_s_table <- renderDT({
      # Owner Share table display

      d <- prep_cost_burden_table(data()$o[['ps']], data()$o[['rs']])
      
      create_dt_cost_burden(table = d, container = container(), source = vals$source) %>%
        formatPercentage(2:9, 1)
    })
    
    output$o_plot <- renderEcharts4r({
      
      echart_rdi(data = plot_data()$o,
                 desc_col = race_ethnicity,
                 str_wrap_num = 20,
                 group = geography_name,
                 x = 'race_ethnicity',
                 y = 'share',
                 ymax = 1,
                 title = 'Cost-Burdened Owner Households (>30%)',
                 egrid_left = "20%")|>
        e_x_axis(formatter = e_axis_formatter("percent", digits = 0))|>
        e_legend(bottom=0) |>
        e_toolbox_feature("dataView") |>
        e_toolbox_feature("saveAsImage")

    })
    
    output$r_map <- renderLeaflet({
      shp <- tract.shape %>%
        filter(census_year == 2010)
      
      d <- create_cost_burden_tract_table()

      create_cost_burden_tract_map(table = d, tenure_type = "Renter", shape_tract = shp, shape_place = map_data())
    })
    
    output$o_map <- renderLeaflet({
      shp <- tract.shape %>%
        filter(census_year == 2010)
      
      d <- create_cost_burden_tract_table()
      
      create_cost_burden_tract_map(table = d, tenure_type = "Owner", shape_tract = shp, shape_place = map_data())
    })
    
    
  }) # end moduleServer
  
}