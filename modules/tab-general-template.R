# Display single tabset containing plot, leaflet map, DT

general_tab_ui <- function(id, subtab_title) {
  ns <- NS(id)
  
  tabPanel(subtab_title,
           fluidRow(
             column(width = 6, 
                    echarts4rOutput(ns("plot")
                    # plotlyOutput(ns("plot")
                    )
             ),
             column(width = 6, 
                    leafletOutput(ns("map")
                    )
             ),
             style = "margin-top: 1rem;"
           ), 
           fluidRow(
             column(width = 12, 
                    DTOutput(ns("table")
                    )
             )
           )
  )
  
}

general_tab_server <- function(id, census_data, place, year, numeric_variables, percent_variables,
                               table_v, plot_v, plot_title, plot_color, 
                               map_v, map_title, map_color, map_value, map_suffix, map_prefix) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    output$table <- renderDT({

      d <- create_summary_table(t = census_data,
                                p = place(),
                                y = year(),
                                v = table_v)
      datatable(
        d,
        rownames = FALSE,
        options = list(pageLength = 15, columnDefs = list(list(className = 'dt-center', targets =1:4)))) %>%
        formatCurrency(numeric_variables, "", digits = 0) %>%
        formatPercentage(percent_variables, 1)

    })

    output$plot <- renderEcharts4r({

      create_summary_echart(d = census_data,
                           p = place(),
                           y = year(),
                           v = plot_v,
                           val="share",
                           f = 100,
                           dec = 1,
                           d.title = plot_title, 
                           s = "%",
                           d.clr = plot_color)

      })

    output$map <- renderLeaflet({
  
      create_tract_map(t = census_data, 
                       v = map_v, 
                       y = year(), 
                       d.clr = map_color,
                       p = place(), 
                       val = map_value,
                       d.title = map_title, 
                       dec = 1, 
                       f = 100, 
                       s = map_suffix,
                       pre = map_prefix)
      })
    
    
  }) # end moduleServer
  
}