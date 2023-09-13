# Define server logic required to draw the map for the main panel
shinyServer(function(input, output, session) {
  
  ## This section is for all the high level stats that are displayed in the sidebar ----
  
  output$Population <- renderText({
    paste("Population: ", return_estimate(t=census_data, p=input$Place, y=input$Year, v="Population",val="estimate",d=-2))
  })
  
  output$MedianAge <- renderText({
    paste("Median Age: ", return_estimate(t=census_data, p=input$Place, y=input$Year, v="Median Age",val="estimate",d=1), " years")
  })
  
  output$MedianIncome <- renderText({
    paste0("Median HH Income: $", return_estimate(t=census_data, p=input$Place, y=input$Year, v="Median HH Income","estimate",-2))
  })
  
  output$AvgHHSize <- renderText({
    paste("HH Size: ", return_estimate(t=census_data, p=input$Place, y=input$Year, v="Avg HH Size",val="estimate",d=2), " people per household")
  })
  
  output$UnempRate <- renderText({
    paste0("Unemployment Rate: ", return_estimate(t=census_data, p=input$Place, y=input$Year, v="Unemployment Rate",val="estimate_percent",d=1),"%")
  })
  
  output$AvgTT <- renderText({
    paste("Travel Time to Work: ", return_estimate(t=census_data, p=input$Place, y=input$Year, v="Avg Travel Time to Work",val="estimate",d=1), " minutes")
  })
  
  output$DisabledShare <- renderText({
    paste0("People with a Disability: ", return_estimate(t=census_data, p=input$Place, y=input$Year, v="With A Disability",val="share",d=1),"%")
  })
  
  output$OwnShare <- renderText({
    paste0("Home Ownership: ", return_estimate(t=census_data, p=input$Place, y=input$Year, v="Own",val="share",d=0),"%")
  })
  
  output$POCShare <- renderText({
    paste0("People of Color: ", as.character((100-as.numeric(return_estimate(t=census_data, p=input$Place, y=input$Year, v="Non-Hispanic White","share",1)))),"%")
  })
  
  output$place_rgeo <- renderText({
    paste("Regional Geography:", find_rgeo_data(p=input$Place, v="regional_geography"))
  }) 
  
  output$place_airaff <- renderText({
    paste("Airport Affected Community:", find_rgeo_data(p=input$Place, v="airport_affected"))
  }) 
  
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
