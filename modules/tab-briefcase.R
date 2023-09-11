# Display job and income metrics (briefcase icon)

briefcase_tab_ui <- function(id) {
  ns <- NS(id)
  intro <- p("The job and income metrics on this page cover the topics of Educational Attainment, Occupation of residents, Industry of residents and Median Income for houesholds. ",
             "Job and income characteristics are summarized in Data Profile 3 (DP03) and Educational Attainment is included in DP02. ",
             "Data profiles are a summarization of a variety of Census Detailed Tables contained within the American Community Survey 
             datasets and are a great resource for high level statistics for a community however detailed information requires the use of specific ACS tables."
  )
  
  div(
    intro,
    tabsetPanel(
      id = ns('tabset'),
      general_tab_ui(id = ns('ea'),
                     subtab_title = "Educational Attainment"),
      general_tab_ui(id = ns('occ'),
                     subtab_title = "Occupation"),
      general_tab_ui(id = ns('ind'),
                     subtab_title = "Industry"),
      general_tab_ui(id = ns('inc'),
                     subtab_title = "Income")
    )
  )
  
}

briefcase_tab_server <- function(id, census_data, year, place, numeric_variables, percent_variables) {

  moduleServer(id, function(input, output, session) { 
    ns <- session$ns

    general_tab_server(id = 'ea', 
                       census_data = census_data, 
                       place = reactive(place()),
                       year = reactive(year()),
                       numeric_variables = numeric_variables, 
                       percent_variables = percent_variables,
                       table_v = "Educational Attainment", 
                       plot_v = "Educational Attainment", 
                       plot_title = "% of Total Population",
                       plot_color = "#91268F",
                       map_v = "College-Degree", 
                       map_title = "College Degree",
                       map_color = "Purples",
                       map_value = "share",
                       map_suffix = "%",
                       map_prefix = "")
    
    general_tab_server(id = 'occ', 
                       census_data = census_data, 
                       place = reactive(place()),
                       year = reactive(year()),
                       numeric_variables = numeric_variables, 
                       percent_variables = percent_variables,
                       table_v = "Occupation", 
                       plot_v = "Occupation", 
                       plot_title = "% of Total Workers",
                       plot_color = "#8CC63E",
                       map_v = "Office", 
                       map_title = "% Office Workers",
                       map_color = "Greens",
                       map_value = "share",
                       map_suffix = "%",
                       map_prefix = "")
    
    general_tab_server(id = 'ind', 
                       census_data = census_data, 
                       place = reactive(place()),
                       year = reactive(year()),
                       numeric_variables = numeric_variables, 
                       percent_variables = percent_variables,
                       table_v = "Industry", 
                       plot_v = "Industry", 
                       plot_title = "% of Total Workers",
                       plot_color = "#F05A28",
                       map_v = "Retail-Accomodations", 
                       map_title = "% Retail-Accom. Workers",
                       map_color = "Oranges",
                       map_value = "share",
                       map_suffix = "%",
                       map_prefix = "")
    
    general_tab_server(id = 'inc', 
                       census_data = census_data, 
                       place = reactive(place()),
                       year = reactive(year()),
                       numeric_variables = numeric_variables, 
                       percent_variables = percent_variables,
                       table_v = "Household Income", 
                       plot_v = "Household Income", 
                       plot_title = "% of Total Households",
                       plot_color = "#00A7A0",
                       map_v = "Income", 
                       map_title = "Median HH Income",
                       map_color = "GnBu",
                       map_value = "estimate",
                       map_suffix = "",
                       map_prefix = "$")
    
  }) # end moduleServer
  
}