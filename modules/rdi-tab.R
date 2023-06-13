# Display UI for RDI tab

rdi_tab_ui <- function(id) {
  ns <- NS(id)
  
  div(
    fluidRow(
      p("A paragraph on the racial disparity index and metrics using CHAS data")
    ),
    fluidRow(
      tabsetPanel(
        rdi_rent_aff_ui(ns('rentaff'))
      ) # end tabsetPanel
    )
    
  ) # end div
  
}

rdi_tab_server <- function(id, shape, place) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    stopifnot(is.reactive(place))
    
    rdi_rent_aff_server('rentaff', 
                        shape = shape,
                        place = place())
    


    
  }) # end moduleServer
  
}