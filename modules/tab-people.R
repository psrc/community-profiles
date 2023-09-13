# Display Age, Race & Ethnicity, Health Coverage and Disability Status (People icon)

people_tab_ui <- function(id) {
  ns <- NS(id)
  intro <- p("The person level metrics on this page cover the topics of Age, Race & Ethnicity, Health Coverage and Disability Status. ",
             "Data Profile 2 (DP02) includes information on People with Disabilites within a community, DP03 includes information of 
              people's access to health coverage and DP05 includes details on Age and Race & Ethnicity. ",
             "Data profiles are a summarization of a variety of Census Detailed Tables contained within the American Community 
              Survey datasets and are a great resource for high level statistics for a community however detailed information requires the use of specific ACS tables."
  )
  
  div(
    intro,
    tabsetPanel(
      id = ns('tabset'),
      general_tab_ui(id = ns('age'),
                     subtab_title = "Age",
                     value = 'age'),
      general_tab_ui(id = ns('race'),
                     subtab_title = "Race & Ethnicity",
                     value = 're'),
      general_tab_ui(id = ns('health'),
                     subtab_title = "Health Coverage"),
      general_tab_ui(id = ns('dis'),
                     subtab_title = "People with a Disability")
    )
  )
  
}

people_tab_server <- function(id, census_data, year, place, numeric_variables, percent_variables) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    general_tab_server(id = 'age', 
                       census_data = census_data, 
                       place = reactive(place()),
                       year = reactive(year()),
                       numeric_variables = numeric_variables, 
                       percent_variables = percent_variables,
                       table_v = "Age", 
                       plot_v = "Age",
                       plot_title = "% of Total Population",
                       plot_color = "#91268F",
                       map_v = "Age", 
                       map_title = "Median Age",
                       map_color = "Purples",
                       map_value = "estimate",
                       # map_f = ,
                       map_suffix = "",
                       map_prefix = "")
    
    general_tab_server(id = 'race',
                       census_data = census_data,
                       place = reactive(place()),
                       year = reactive(year()),
                       numeric_variables = numeric_variables,
                       percent_variables = percent_variables,
                       table_v = "Race",
                       plot_v = "Race",
                       plot_title = "% of Total Population",
                       plot_color = "#8CC63E",
                       map_v = "People-of-Color",
                       map_title = "People of Color",
                       map_color = "Greens",
                       map_value = "share",
                       map_f = 100,
                       map_suffix = "%",
                       map_prefix = "")
    
    general_tab_server(id = 'health',
                       census_data = census_data,
                       place = reactive(place()),
                       year = reactive(year()),
                       numeric_variables = numeric_variables,
                       percent_variables = percent_variables,
                       table_v = "Health Coverage",
                       plot_v = "Health Coverage",
                       plot_title = "% of Total Population",
                       plot_color = "#F05A28",
                       map_v = "Health-Insurance",
                       map_title = "No Health Coverage",
                       map_color = "Oranges",
                       map_value = "share",
                       map_f = 100,
                       map_suffix = "%",
                       map_prefix = "")
    
    general_tab_server(id = 'dis',
                       census_data = census_data,
                       place = reactive(place()),
                       year = reactive(year()),
                       numeric_variables = numeric_variables,
                       percent_variables = percent_variables,
                       table_v = "Disability",
                       plot_v = "Disability",
                       plot_title = "% of Total Population",
                       plot_color = "#00A7A0",
                       map_v = "Disabled",
                       map_title = "People with a Disability",
                       map_color = "GnBu",
                       map_value = "share",
                       map_f = 100,
                       map_suffix = "%",
                       map_prefix = "")
    
  }) # end moduleServer
  
}