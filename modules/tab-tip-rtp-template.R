# Display single tabset containing leaflet map, DT (primarily for tip/rtp)

tip_rtp_tab_ui <- function(id, subtab_title, intro_text, value = NULL) {
  ns <- NS(id)
  
  tabPanel(subtab_title,
           value = value,
           fluidRow(
             column(width = 6, 
                    intro_text
                    ),
             column(width = 6, 
                    leafletOutput(ns("map"))
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

tip_rtp_tab_server <- function(id, place, plan_year, type = c('tip', 'rtp')) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    output$table <- renderDT({
      
      d <- create_project_table(p = place(),
                                i = projects.shape,
                                f = final.nms,
                                plan.yr = plan_year)
      
      datatable(d, 
                rownames = FALSE, 
                options = list(pageLength = 10, columnDefs = list(list(className = 'dt-center', targets = 4:6)))) %>% 
        formatCurrency(currency.rtp , "$", digits = 0)
      
    })
    
    output$map <- renderLeaflet({
      if(type == 'tip') {
        
        m <- create_tip_map(p = place(), 
                       plan.yr = plan_year, 
                       d.title = "Transportation Improvement Program")
      } else {
        m <-  create_rtp_map(p = place(), 
                             plan.yr = plan_year, 
                             d.title = "Regional Transportation Plan")
      }
     
      return(m)
    })
    
    
  }) # end moduleServer
  
}