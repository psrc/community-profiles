# Display Mode Share, Travel Time and Time of Departure for Work related travel as well as the number of Vehicles Available for households (car icon)

car_tab_ui <- function(id) {
  ns <- NS(id)
  intro <- p("The travel related metrics on this page cover the topics of Mode Share, Travel Time and Time of Departure for 
             Work related travel as well as the number of Vehicles Available for households in the community. ",
             "Mode Share and Vehicle Availability are metrics from Data Profile 3 (DP03) and DP04 respectively. 
             Detailed Travel Time comes from table B08303 and Departure Time is from table B08302. ",
             "Data profiles are a summarization of a variety of Census Detailed Tables contained within the American Community Survey 
             datasets and are a great resource for high level statistics for a community however detailed information requires the 
             use of specific ACS tables."
  )
  
  div(
    intro,
    tabsetPanel(
      id = ns('tabset'),
      general_tab_ui(id = ns('mode'),
                     subtab_title = "Mode Share to Work"),
      general_tab_ui(id = ns('travelTime'),
                     subtab_title = "Travel Time to Work"),
      general_tab_ui(id = ns('departTime'),
                     subtab_title = "Departure Time to Work"),
      general_tab_ui(id = ns('veh'),
                     subtab_title = "Vehicles Available")
    )
  )
  
}

car_tab_server <- function(id, census_data, year, place, numeric_variables, percent_variables) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    general_tab_server(id = 'mode', 
                       census_data = census_data, 
                       place = reactive(place()),
                       year = reactive(year()),
                       numeric_variables = numeric_variables, 
                       percent_variables = percent_variables,
                       table_v = "Mode Share", 
                       plot_v = "Mode Share",
                       plot_title = "% of Total Workers",
                       plot_color = "#91268F",
                       map_v = "Non-Vehicle", 
                       map_title = "Non-Vehicle Modes to Work",
                       map_color = "Purples",
                       map_value = "share",
                       map_f = 100,
                       map_suffix = "%",
                       map_prefix = "")
    
    general_tab_server(id = 'travelTime', 
                       census_data = census_data, 
                       place = reactive(place()),
                       year = reactive(year()),
                       numeric_variables = numeric_variables, 
                       percent_variables = percent_variables,
                       table_v = "Travel Time", 
                       plot_v = "Travel Time",
                       plot_title = "% of Total Workers",
                       plot_color = "#8CC63E",
                       map_v = "Time", 
                       map_title = "Travel Time to Work",
                       map_color = "Greens",
                       map_value = "estimate",
                       map_f = 1,
                       map_suffix = "",
                       map_prefix = "")
    
    general_tab_server(id = 'departTime', 
                       census_data = census_data, 
                       place = reactive(place()),
                       year = reactive(year()),
                       numeric_variables = numeric_variables, 
                       percent_variables = percent_variables,
                       table_v = "Departure Time", 
                       plot_v = "Departure Time",
                       plot_title = "% of Total Workers",
                       plot_color = "#F05A28",
                       map_v = "AM-Peak", 
                       map_title = "AM Peak Departure",
                       map_color = "Oranges",
                       map_value = "share",
                       map_f = 100,
                       map_suffix = "%",
                       map_prefix = "")
    
    general_tab_server(id = 'veh', 
                       census_data = census_data, 
                       place = reactive(place()),
                       year = reactive(year()),
                       numeric_variables = numeric_variables, 
                       percent_variables = percent_variables,
                       table_v = "Vehicle Availability", 
                       plot_v = "Vehicle Availability",
                       plot_title = "% of Total Households",
                       plot_color = "#00A7A0",
                       map_v = "Zero-Car", 
                       map_title = "Zero Car HH's",
                       map_color = "GnBu",
                       map_value = "share",
                       map_f = 100,
                       map_suffix = "%",
                       map_prefix = "")
    
  }) # end moduleServer
  
}