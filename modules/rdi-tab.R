# Display UI for RDI tab

rdi_tab_ui <- function(id) {
  ns <- NS(id)
  
  div(
    fluidRow(
      p("The household level metrics on this page can be used to support analyses of racially disparate impacts (RDI) within a local jurisdiction or community. 
        Topics include racial disparities in Cost Burden, Rental Affordability, Income, and Tenure. 
        The data were collected from the", 
        tags$a(href="https://www.huduser.gov/portal/datasets/cp.html","Comprehensive Housing Affordability Strategy (CHAS) dataset", target = "_blank"), ".",  
        "The People section of this dashboard includes a related measure for Race & Ethnicity. The Washington Department of Commerce has", tags$a(href="https://deptofcommerce.app.box.com/s/1l217l98jattb87qobtw63pkplzhxege", "published guidance", target = "_blank"), 
        "for jurisdictions looking to address racially disparate impacts in their comprehensive plan update.")
    ),
    fluidRow(
      tabsetPanel(id = ns('tabset'),
                  rdi_cost_burden_ui(ns('costburden')),
                  rdi_rentaff_ui(ns('rentaff')),
                  rdi_income_ui(ns('income')),
                  rdi_tenure_ui(ns('tenure')),
                  footer = p("Race and Hispanic/Latinx origin are reported as overlapping categories in CHAS data metrics. For more information, 
         see the following resources on the US Census Bureau’s website:", tags$a(href="https://www.census.gov/topics/population/race/about.html", "About the Topic of Race"), "and", 
         tags$a(href="https://www.census.gov/topics/population/hispanic-origin/about.html", "About the Hispanic Population and its Origin"), 
         style = "font-size: 10pt; margin-top: 2rem;")
                  
                  
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