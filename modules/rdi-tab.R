# Display UI for RDI tab

rdi_tab_ui <- function(id) {
  ns <- NS(id)
  
  div(
    fluidRow(
      p("A paragraph on the racial disparity index and metrics using CHAS data")
    ),
    fluidRow(
      tabsetPanel(id = ns('tabset'),
        rdi_metric_ui(ns('rentaff'), 'Rental Affordability')
        
      ) # end tabsetPanel
    )
    
  ) # end div
  
}

rdi_tab_server <- function(id, shape, place) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    place <- reactive(place())

    rdi_metric_server('rentaff', 
                      shape = shape,
                      place = place)
   
  }) # end moduleServer
  
}