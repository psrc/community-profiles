# Display UI for RDI tab

rdi_tab_ui <- function(id) {
  ns <- NS(id)
  
  div(
    fluidRow(
      p("A paragraph on the racial disparity index and metrics using CHAS data")
    ),
    fluidRow(
      tabsetPanel(id = ns('tabset'),
        rdi_rentaff_ui(ns('rentaff'))
        
      ) # end tabsetPanel
    )
    
  ) # end div
  
}

rdi_tab_server <- function(id, shape, place) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    rdi_rentaff_server('rentaff', 
                      shape = shape,
                      place = reactive(place())
                      )
   
  }) # end moduleServer
  
}