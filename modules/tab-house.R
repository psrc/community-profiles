# Display  Housing Type, Home Values, Monthly Rental Cost and Home Ownership (House icon)

house_tab_ui <- function(id) {
  ns <- NS(id)
  intro <- p("The housing and household level metrics on this page cover the topics of Housing Type, Home Values, Monthly Rental 
              Cost and Home Ownership. ",
             "Data Profile 4 (DP04) includes a wealth of information on housing and household characteristics. ",
             "Data profiles are a summarization of a variety of Census Detailed Tables contained within the American Community 
              Survey datasets and are a great resource for high level statistics for a community however detailed information 
              requires the use of specific ACS tables."
  )
  
  div(
    intro,
    tabsetPanel(
      id = ns('tabset'),
      general_tab_ui(id = ns('hu'),
                     subtab_title = "Housing Units",
                     value = 'units'),
      general_tab_ui(id = ns('homeVal'),
                     subtab_title = "Home Value"),
      general_tab_ui(id = ns('rent'),
                     subtab_title = "Monthly Rent"),
      general_tab_ui(id = ns('own'),
                     subtab_title = "Home Ownership")
    )
  )
  
}

house_tab_server <- function(id, census_data, year, place, numeric_variables, percent_variables) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    general_tab_server(id = 'hu', 
                       census_data = census_data, 
                       place = reactive(place()),
                       year = reactive(year()),
                       numeric_variables = numeric_variables, 
                       percent_variables = percent_variables,
                       table_v = "Housing Type", 
                       plot_v = "Housing Type",
                       plot_title = "% of Total Housing Units",
                       plot_color = "#91268F",
                       map_v = "Middle-Housing", 
                       map_title = "Medium Density Housing",
                       map_color = "Purples",
                       map_value = "share",
                       map_f = 100,
                       map_suffix = "%",
                       map_prefix = "")
    
    general_tab_server(id = 'homeVal',
                       census_data = census_data,
                       place = reactive(place()),
                       year = reactive(year()),
                       numeric_variables = numeric_variables,
                       percent_variables = percent_variables,
                       table_v = "Home Value",
                       plot_v = "Home Value",
                       plot_title = "% of Total Ownership Units",
                       plot_color = "#8CC63E",
                       map_v = "Home-Value",
                       map_title = "Median Value",
                       map_color = "Greens",
                       map_value = "estimate",
                       map_suffix = "",
                       map_prefix = "$")
    
    general_tab_server(id = 'rent',
                       census_data = census_data,
                       place = reactive(place()),
                       year = reactive(year()),
                       numeric_variables = numeric_variables,
                       percent_variables = percent_variables,
                       table_v = "Monthly Rent",
                       plot_v = "Monthly Rent",
                       plot_title = "% of Total Rental Units",
                       plot_color = "#F05A28",
                       map_v = "Rent",
                       map_title = "Median Rent",
                       map_color = "Oranges",
                       map_value = "estimate",
                       map_f = 1,
                       map_suffix = "",
                       map_prefix = "$")

    general_tab_server(id = 'own',
                       census_data = census_data,
                       place = reactive(place()),
                       year = reactive(year()),
                       numeric_variables = numeric_variables,
                       percent_variables = percent_variables,
                       table_v = "Home Ownership",
                       plot_v = "Home Ownership",
                       plot_title = "% of Total Households",
                       plot_color = "#00A7A0",
                       map_v = "Home-Owner",
                       map_title = "% Home Ownership",
                       map_color = "GnBu",
                       map_value = "share",
                       map_f = 100,
                       map_suffix = "%",
                       map_prefix = "")
    
  }) # end moduleServer
  
}