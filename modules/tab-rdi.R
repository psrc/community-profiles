# Display UI for RDI tab

rdi_tab_ui <- function(id) {
  ns <- NS(id)
  
  intro <- p("The housing and household level metrics on this page can be used to support analyses of racially disparate impacts (RDI) within a local jurisdiction or community. 
        Topics include racial disparities in Cost Burden, Rental Affordability, Income, Tenure, and Displacement Risk. 
        The data were drawn from HUD's", 
        tags$a(href="https://www.huduser.gov/portal/datasets/cp.html","Comprehensive Housing Affordability Strategy (CHAS) dataset,", target = "_blank"), 
        " a custom American Community Survey data product. 
        The People section of this dashboard includes a related measure for", actionLink(ns("link_re"), "Race & Ethnicity"),". Additional
        housing measures are also available in the", actionLink(ns("link_hh"), "Households & Housing"),"section. The Washington Department of Commerce has", tags$a(href="https://deptofcommerce.app.box.com/s/1l217l98jattb87qobtw63pkplzhxege", "published guidance", target = "_blank"), 
        "on analyzing disparate impacts in comprehensive plans updates.")
  
  footer <- p(tags$b("Caution Flag for Small Populations"),": The American Community Survey (ACS) and HUD's Comprehensive Housing Affordability Strategy (CHAS) estimates are based 
             on a sample of the population and have sampling error. Sampling error is the difference between the sample value and the population value (if one were to 
             survey the entire population). To help users understand the degree of sample error in a given estimate, the data sources publish a Margin of Error for 
             every estimate. The Margin of Error allows the user to assess the reliability of the estimates.")
  
  footer02 <- p("To determine margins of error for estimates of interest, data users may refer directly to the published ", tags$a(href="https://data.census.gov/", "ACS"), 
                " and ",  tags$a(href="https://www.huduser.gov/portal/datasets/cp.html", "CHAS"), " data tables or reach out to ", tags$a(href="https://www.psrc.org/", "PSRC"), " for assistance, we would be happy to help you.")
  
  disclaimer <- p("Please allow at least 5 seconds for visuals to load or update.", style = "font-size: 10pt; margin-top: 2rem;")
  
  div(
    fluidRow(
      intro,
      disclaimer
    ),
    fluidRow(
      tabsetPanel(id = ns('tabset'),
                  rdi_cost_burden_ui(ns('costburden')),
                  rdi_rentaff_ui(ns('rentaff')),
                  rdi_income_ui(ns('income')),
                  rdi_tenure_ui(ns('tenure')),
                  rdi_disp_risk_ui(ns('disprisk')),
                  footer = div(div(footer, style = "font-size: 10pt; margin: 1rem 0;"), 
                               div(footer02, style = "font-size: 10pt; margin: 1rem 0;"))
      ) # end tabsetPanel
    )
    
  ) # end div
  
  
}

rdi_tab_server <- function(id, shape, place, disp_risk_shape) {
  
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
    
    rdi_disp_risk_server('disprisk',
                         shape = shape,
                         place = reactive(place()),
                         disp_risk_shape = disp_risk_shape)
    
  }) # end moduleServer
  
}