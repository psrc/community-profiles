# Define server logic required to draw the map for the main panel
shinyServer(function(input, output, session) {
  
  ## This section is for all the high level stats that are displayed in the sidebar ----
  juris_profile_server('profile', 
                       place = reactive(input$Place), 
                       year = reactive(input$Year))
  
  
  output$downloadData <- downloadHandler(
    filename = function() {paste0(tolower(str_replace_all(input$Year," ","-")),"-",tolower(str_replace_all(input$Place," ","-")),".xlsx")},
    content <- function(file) {file.copy(here(paste0("data-profiles/",tolower(str_replace_all(input$Year," ","-")),"-",tolower(str_replace_all(input$Place," ","-")),".xlsx")),file)},
    contentType = "application/Excel"
  )
  
  ## This section is for all the data on the Main Overview Page ----

  home_tab_server("home", place = reactive({input$Place}))
  
  # people tab ----
  ## Age, Race & Ethnicity, Health Coverage and Disability Status
  
  people_tab_server("people",
                    census_data = census_data, 
                    place = reactive(input$Place),
                    year = reactive(input$Year),
                    numeric_variables = numeric_variables, 
                    percent_variables = percent_variables)
  
  # house section ----
  ## Housing Type, Home Values, Monthly Rental Cost and Home Ownership
  
  house_tab_server(id = "house", 
                   census_data = census_data, 
                   place = reactive(input$Place),
                   year = reactive(input$Year),
                   numeric_variables = numeric_variables, 
                   percent_variables = percent_variables)
  
  # RDI ----
  
  rdi_tab_server("rdi", 
                 shape = community.shape,
                 place = reactive({input$Place}))
  
  # link from RDI to Race & Ethnicity tab in People/demographics
  observeEvent(input$`rdi-link_re`, {
    updateTabsetPanel(session, inputId = 'Navbar', selected = 'people')
    updateTabsetPanel(session, inputId = input$`people-tabset`, selected = input$`people-re`) #'tab_people''re'
  })
  
  # link from RDI to Households and Housing
  observeEvent(input$`rdi-link_hh`, {
    updateTabsetPanel(session, inputId = 'Navbar', selected = 'housing')
    updateTabsetPanel(session, inputId = input$`house-tabset`, selected = input$`house-units`)
  })
  
  # briefcase section ----
  ## Educational Attainment, Occupation/Industry of residents, Median HH Income

  briefcase_tab_server(id = "briefcase", 
                       census_data = census_data, 
                       place = reactive(input$Place),
                       year = reactive(input$Year),
                       numeric_variables = numeric_variables, 
                       percent_variables = percent_variables)
  
  # car section ----
  ## Mode Share, Travel Time and Time of Departure for Work, Vehicles Available
  
  car_tab_server(id = "car", 
                  census_data = census_data, 
                  place = reactive(input$Place),
                  year = reactive(input$Year),
                  numeric_variables = numeric_variables, 
                  percent_variables = percent_variables)
  
  # wrench section ----
  ## TIP, RTP
  
  wrench_tab_server(id = "wrench", 
                    place = reactive(input$Place))

  
})
