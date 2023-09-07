# Display single tabset containing plot, leaflet map, DT

general_tab_ui <- function(id, subtab_title) {
  ns <- NS(id)
  
  tabPanel(subtab_title,
           fluidRow(
             column(width = 6, 
                    plotlyOutput(ns("plot")
                    )
             ),
             column(width = 6, 
                    leafletOutput(ns("map")
                    )
             )
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
                               table_v, plot_v, map_v, map_title) {
  # input$Place input$Year
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    # output$table <- renderDT({
    #   
    #   d <- create_summary_table(t = census_data,
    #                             p = place(),
    #                             y = year(),
    #                             v = table_v)
    #   datatable(
    #     d,
    #     rownames = FALSE, 
    #     options = list(pageLength = 15, columnDefs = list(list(className = 'dt-center', targets =1:4)))) %>% 
    #     formatCurrency(numeric_variables, "", digits = 0) %>% 
    #     formatPercentage(percent_variables, 1)
    #   
    # })
    # 
    # output$plot <- renderPlotly({
    #   
    #   create_summary_chart(d = census_data, 
    #                        p = place(), 
    #                        y = year(), 
    #                        v = plot_v, 
    #                        val="share", 
    #                        f = 100, 
    #                        dec = 1, 
    #                        d.title = "% of Total Population",
    #                        s = "%",
    #                        d.clr = "#91268F")
    #   
    #   })

    output$map <- renderLeaflet({
  
      create_tract_map(t = census_data, 
                       v = map_v, 
                       y = year(), 
                       d.clr = "Purples", 
                       p = place(), 
                       val = "share", 
                       d.title = map_title, 
                       dec = 1, 
                       f = 100, 
                       s = "%")
      })
    
    
  }) # end moduleServer
  
}