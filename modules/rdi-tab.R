# Display UI for RDI tab

rdi_tab_ui <- function(id) {
  ns <- NS(id)
  
  div(
    fluidRow(
      p("A paragraph on the racial disparity index and metrics using CHAS data")
    ),
    fluidRow(
      tabsetPanel(id = ns('tabset'),
        rdi_rentaff_ui(ns('rentaff'))#,
        # rdi_tenure_ui(ns('tenure'))
        
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
    
    # rdi_tenure_server('tenure', 
    #                    shape = shape,
    #                    place = reactive(place())
    # )
   
  }) # end moduleServer
  
}