# Display data sources page

source_tab_ui <- function(id) {
  ns <- NS(id)
  
  census_sources <- c("Travel Time to Work: Table B08303",
                    "Departure Time to Work: Table B08302",
                    "Age: Data Profile 5 (DP05)",
                    "Disability: Data Profile 2 (DP02)",
                    "Housing Units: Data Profile 4 (DP04)",
                    "Home Value: Data Profile 4 (DP04)",
                    "Income: Data Profile 3 (DP03)",
                    "Industry: Data Profile 3 (DP03)",
                    "Mode Share: Data Profile 3 (DP03)",
                    "Monthly Rent: Data Profile 4 (DP04)",
                    "Occupation: Data Profile 3 (DP03)",
                    "Race: Data Profile 5 (DP05)",
                    "Vehicles Available: Data Profile 4 (DP04)")
  
  census_about <- p("The Census Data used in this portal is stored in PSRC's central database but is available from the US Census Bureau.
                    All tables can be downloaded either via the", 
                    tags$a(href="https://www.census.gov/data/developers/data-sets/acs-5year.html", "Census API", target="_blank"), 
                    "or the", tags$a(href="https://data.census.gov/cedsci/", "Census Data", target="_blank"), "page.")
  
  chas_about <- p("The housing and household level metrics in the Racially Disparate Impacts section of this portal 
                  were developed using the U.S. Department of Housing and Urban Development’s Comprehensive Housing 
                  Affordability Strategy (CHAS) dataset. All CHAS tables can be downloaded from the ",
                  tags$a(href="https://www.huduser.gov/portal/datasets/cp.html", "huduser data portal.", target="_blank"))
  
  chas_sources <- c("Table 1. Occupied Housing Units by Tenure, 1 of 4 Housing Problems, Household Income, and Race/Ethnicity",
                    "Table 8. Occupied Housing Units by Tenure, Household Income, Cost Burden, and Kitchen and Plumbing Facilities",
                    "Table 9. Occupied Housing Units by Tenure, Race/Ethnicity, and Cost Burden",
                    "Table 14b. Vacant-for-Rent Housing Units by Kitchen and Plumbing Facilities, Rent Category, and Number of Bedrooms",
                    "Table 15c. Renter Occupied Housing Units by Kitchen and Plumbing Facilities, Rent Category, Household Income, and Number of Bedrooms")
  
  
  tabPanel(icon("info-circle"),
           h1("Data Sources"),
           p("The data in this portal comes from a few key sources:"),
           h2("Census Data"),
           census_about,
           h3("Census Tables:"),
           
           htmltools::withTags(
             lapply(census_sources, function(x) div(x, style = "margin-bottom: .2rem;"))
           ),
           
           h2('CHAS Data'),
           chas_about,
           h3('CHAS Tables:'),
           
           htmltools::withTags(
             lapply(chas_sources, function(x) div(x, style = "margin-bottom: .2rem;"))
           )
           
  ) # end tabPanel
}

# source_tab_server <- function(id) {
#   
#   moduleServer(id, function(input, output, session) { 
#     ns <- session$ns
#     
#   }) # end moduleServer
#   
# }