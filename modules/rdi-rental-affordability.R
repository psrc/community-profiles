# Display Rental Affordability metric

rdi_rentaff_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(title = "Rental Affordability",
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
      # pull (currently from Elmer) semi-prepped CHAS

      df <- create_rental_affordability_table(juris = 'place') %>% 
        filter(geography_name == place())

      df_region <- create_rental_affordability_table(juris = 'region')

      return(list(place = df, region = df_region))
    })
    
    table_data <- reactive({
      # data in wide for display in table
      
      region <- data()$region %>% 
        select(description, renter_hh_income, rental_units, ends_with('share')) %>% 
          rename_with(~paste0(.x, '_reg'))
        
      d <- left_join(data()$place, region, by = c('description' = 'description_reg')) %>% 
        select(description, renter_hh_income, rental_units, ends_with('share'), ends_with('reg'))
    })
    
    plot_data <- reactive({
      # data in long form for plotting
      
      df <- bind_rows(data()) %>% 
        filter(description != 'All') %>% 
        select(chas_year, geography_name, description, ends_with('share')) %>% 
        pivot_longer(cols = ends_with('Share'),
                     names_to = 'type',
                     values_to = 'value')
    })
    
    plot_clean_data <- reactive({
      # munge long form data for visual
      
      geog <- c(place(), 'Region')

      plot_data() %>% 
        mutate(type_desc = case_when(type == 'rental_units_share' ~ 'Rental Units', 
                                     type == 'renter_hh_income_share' ~ 'Households')) %>% 
        mutate(description_short = case_when(description == 'Extremely Low Income (<30% AMI)' ~ '<30% AMI',
                                             description == 'Very Low Income (30-50% AMI)' ~ '30-50% AMI',
                                             description == 'Low Income (50-80% AMI)' ~ '50-80% AMI',
                                             description == 'Moderate Income (80-100% AMI)' ~ '80-100% AMI',
                                             description == 'Greater than 100% of AMI' ~ '> 100% AMI'),
               geography_name = factor(geography_name, levels = geog)) %>%
        mutate(description_short = factor(description_short, levels = c('> 100% AMI', '80-100% AMI', '50-80% AMI', '30-50% AMI', '<30% AMI'))) %>% 
        arrange(description_short)
    })
    
    place_name <- reactive({unique(data()$place$geography_name)})
    
    container <- reactive({
      # custom container for DT
      
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
      # table display
      
      source <- 'Sources: US HUD, 2015-2019 Comprehensive Housing Affordability Strategy (CHAS) Tables 8, 14B, 15C'
     
      datatable(table_data(),
                container = container(),
                rownames = FALSE,
                options = list(columnDefs = list(list(className = 'dt-center', targets = 1:8))),
                caption = htmltools::tags$caption(
                  style = 'caption-side: bottom; text-align: right;',
                  htmltools::em(source)
                )) %>% 
        formatPercentage(str_subset(colnames(table_data()), ".*share(.)*$"), 1)
    })
    
    echart_ra <- function(data, filter_type, group, x, y, title) {
      data %>%
        filter(type == filter_type) %>%
        mutate(description_short = str_wrap(description_short, 6)) %>%
        group_by({{group}}) %>%
        e_charts_(x = x) |>
        e_bar_(y) |>
        e_x_axis(axisLabel = list(interval = 0L),
                 axisTick = list(alignWithLabel = TRUE)) |>
        e_flip_coords() |>
        e_grid(left = "20%", top = '10%') |>
        e_title(text = title, 
                left = 'center',
                textStyle = list(fontSize = 12)) |>
        e_color(psrc_colors$obgnpgy_5) %>% 
        e_tooltip(formatter =  e_tooltip_item_formatter("percent")) |> #,  trigger = "axis"
        e_x_axis(formatter = e_axis_formatter("percent", digits = 0))
      
    }
    
    output$plot01 <- renderEcharts4r({
      echart_ra(data = plot_clean_data(),
                filter_type = "renter_hh_income_share",
                group = geography_name,
                x = 'description_short',
                y = 'value',
                title = 'Households') |> 
        e_legend(bottom=0) |>
        e_group("grp") 
      
    })
    
    output$plot02 <- renderEcharts4r({
      echart_ra(data = plot_clean_data(),
                filter_type = "rental_units_share",
                group = geography_name,
                x = 'description_short',
                y = 'value',
                title = 'Rental Units') |>  
        e_legend(show=FALSE) |> 
        e_toolbox_feature("dataView") |> 
        e_toolbox_feature("saveAsImage") |> 
        e_group("grp") |> 
        e_connect_group("grp")
    })
    
    map_data <- reactive({
      s <- shape %>% filter(geog_name == place())
    })
    
    output$map <- renderLeaflet({
      shp <- tract.shape %>% 
        filter(census_year == 2010)
      
      d <- create_rental_affordability_tract_table()
      
      s <- shp %>%
        left_join(d, by = c('geoid' = 'tract_geoid'))
      
      m <- create_chas_tract_map(shape_tract = s,
                                 shape_place = map_data(), 
                                 title = paste('Census Tracts of Rental Units', 'Less than 80% AMI', sep = "<br>"))
    })
    
    
  }) # end moduleServer
  
}