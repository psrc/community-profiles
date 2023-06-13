# Display Rental Affordability metric

rdi_rent_aff_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel("Rental Affordability",
           fluidRow(
             column(6,
                    plotOutput(ns('plot'))),

             leaflet_ui(ns('rentaff_map'))
             
           ), # end fluidrow
           fluidRow(
             # insert DT module
             column(width = 12,
                    DTOutput(ns("table"))
             )
           ) # end fluidRow
  ) # end tabpanel
  
}

rdi_rent_aff_server <- function(id, shape, place) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    output$table <- renderDT(datatable(mtcars))
    
    output$plot <- renderPlot(
      ggplot(mtcars) +
        geom_point(aes(mpg, cyl))
    )
    

    leaflet_server('rentaff_map', 
                   shape = shape,
                   place = place())

    
    
  }) # end moduleServer
  
}