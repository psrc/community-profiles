# Display data sources page

source_tab_ui <- function(id) {
  ns <- NS(id)
  
  data_sources <- c("Travel Time to Work: Table B08303",
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
  
  tabPanel(icon("info-circle"),
           h1("Data Sources"),
           p("The data in this portal comes from a few key sources:"),
           h2("Census Data"),
           census_about,
           h3("Census Tables:"),
           
           htmltools::withTags(
             lapply(data_sources, function(x) div(x, style = "margin-bottom: .2rem;"))
           )
           
           # htmltools::withTags(
           #   lapply(data_sources, p)
           # )
           
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