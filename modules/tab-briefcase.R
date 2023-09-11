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
                     subtab_title = "Educational Attainment")
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
                       map_v = "College-Degree", 
                       map_title = "College Degree")
    
    
  }) # end moduleServer
  
}