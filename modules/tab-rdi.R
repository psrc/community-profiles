# Display UI for RDI tab

rdi_tab_ui <- function(id) {
  ns <- NS(id)
  
  div(
    fluidRow(
      p("The housing and household level metrics on this page can be used to support analyses of racially disparate impacts (RDI) within a local jurisdiction or community. 
        Topics include racial disparities in Cost Burden, Rental Affordability, Income, and Tenure. 
        The data were drawn from HUD's", 
        tags$a(href="https://www.huduser.gov/portal/datasets/cp.html","Comprehensive Housing Affordability Strategy (CHAS) dataset,", target = "_blank"), 
        " a custom American Community Survey data product. 
        The People section of this dashboard includes a related measure for", actionLink(ns("link_re"), "Race & Ethnicity"),". Additional
        housing measures are also available in the", actionLink(ns("link_hh"), "Households & Housing"),"section. The Washington Department of Commerce has", tags$a(href="https://deptofcommerce.app.box.com/s/1l217l98jattb87qobtw63pkplzhxege", "published guidance", target = "_blank"), 
        "on analyzing disparate impacts in comprehensive plan updates."),
      p("Please allow at least 5 seconds for visuals to load.", style = "font-size: 10pt; margin-top: 2rem;")
    ),
    fluidRow(
      tabsetPanel(id = ns('tabset'),
                  rdi_cost_burden_ui(ns('costburden')),
                  rdi_rentaff_ui(ns('rentaff')),
                  rdi_income_ui(ns('income')),
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
    
    rdi_cost_burden_server('costburden',
                           shape = shape,
                           place = reactive(place())
    )
    
    rdi_income_server('income',
                      shape = shape,
                      place = reactive(place())
    )
    
  }) # end moduleServer
  
}