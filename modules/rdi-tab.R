# Display UI for RDI tab

rdi_tab_ui <- function(id) {
  ns <- NS(id)
  
  div(
    fluidRow(
      p("The person level metrics on this page address Racially Disparate Impacts (RDI) throughout the PSRC region. 
        Topics include Racial Composition and racial disparities in Cost Burden, Rental Affordability, Income, and Tenure. 
        The data were collected from the", 
        tags$a(href="https://www.huduser.gov/portal/datasets/cp.html","2015-2019 Comprehensive Housing Affordability Strategy (CHAS)", target = "_blank"), ".",  
        "The Washington Department of Commerce has", tags$a(href="https://deptofcommerce.app.box.com/s/1l217l98jattb87qobtw63pkplzhxege", "published guidance", target = "_blank"), 
        "for jurisdictions looking to address RDI in their comprehensive plan update.")
    ),
    fluidRow(
      tabsetPanel(id = ns('tabset'),
        rdi_rentaff_ui(ns('rentaff')),
        rdi_tenure_ui(ns('tenure'))
        
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
    
    rdi_tenure_server('tenure',
                       shape = shape,
                       place = reactive(place())
    )
   
  }) # end moduleServer
  
}