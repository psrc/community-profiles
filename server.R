# Define server logic required to draw the map for the main panel
shinyServer(function(input, output, session) {
  
  # RDI ----
  
  rdi_tab_server("rdi", 
                 shape = community.shape,
                 place = reactive({input$Place}))
  
  # link from RDI to Race & Ethnicity tab in People/demographics
  observeEvent(input$`rdi-link_re`, {
    updateTabsetPanel(session, inputId = 'Navbar', selected = 'people')
    updateTabsetPanel(session, inputId = 'tab_people', selected = 're')
  })
  
  # link from RDI to Households and Housing
  observeEvent(input$`rdi-link_hh`, {
    updateTabsetPanel(session, inputId = 'Navbar', selected = 'housing')
    updateTabsetPanel(session, inputId = input$`house-tabset`, selected = input$`house-units`)
  })
  
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
  
  ## Age Tab Panel Information ----
  
  output$table_age <- DT::renderDataTable({
    datatable(create_summary_table(t=census_data,p=input$Place,y=input$Year,v="Age"),rownames = FALSE, options = list(pageLength = 15, columnDefs = list(list(className = 'dt-center', targets =1:4)))) %>% formatCurrency(numeric_variables, "", digits = 0) %>% formatPercentage(percent_variables, 1)
  })
  
  output$plot_age <- renderPlotly({create_summary_chart(d=census_data, p=input$Place, y=input$Year, v="Age", val="share", f=100, dec=1, d.title="% of Total Population",s="%",d.clr="#91268F")})
  
  output$age_map <- renderLeaflet({create_tract_map(t=census_data, v="Age", y=input$Year, d.clr="Purples", p=input$Place, val="estimate", d.title="Median Age", dec=1)})
  
  ## Race Tab Panel Information ----
  
  output$table_race <- DT::renderDataTable({
    datatable(create_summary_table(t=census_data,p=input$Place,y=input$Year,v="Race"),rownames = FALSE, options = list(pageLength = 15, columnDefs = list(list(className = 'dt-center', targets =1:4)))) %>% formatCurrency(numeric_variables, "", digits = 0) %>% formatPercentage(percent_variables, 1)
  })
  
  output$plot_race <- renderPlotly({create_summary_chart(d=census_data, p=input$Place, y=input$Year, v="Race", val="share", f=100, dec=1, d.title="% of Total Population",s="%",d.clr="#8CC63E")})
  
  output$race_map <- renderLeaflet({create_tract_map(t=census_data, v="People-of-Color", y=input$Year, d.clr="Greens", p=input$Place, val="share", d.title="People of Color", dec=1, f=100, s="%")})
  
  ## Health Insurance Tab Panel Information ----
  
  output$table_health <- DT::renderDataTable({
    datatable(create_summary_table(t=census_data,p=input$Place,y=input$Year,v="Health Coverage"),rownames = FALSE, options = list(pageLength = 15, columnDefs = list(list(className = 'dt-center', targets =1:4)))) %>% formatCurrency(numeric_variables, "", digits = 0) %>% formatPercentage(percent_variables, 1)
  })
  
  output$plot_health <- renderPlotly({create_summary_chart(d=census_data, p=input$Place, y=input$Year, v="Health Coverage", val="share", f=100, dec=1, d.title="% of Total Population",s="%",d.clr="#F05A28")})
  
  output$health_map <- renderLeaflet({create_tract_map(t=census_data, v="Health-Insurance", y=input$Year, d.clr="Oranges", p=input$Place, val="share", d.title="No Health Coverage", dec=1, f=100, s="%")})
  
  ## Disability Tab Panel Information ----
  
  output$table_disability <- DT::renderDataTable({
    datatable(create_summary_table(t=census_data,p=input$Place,y=input$Year,v="Disability"),rownames = FALSE, options = list(pageLength = 15, columnDefs = list(list(className = 'dt-center', targets =1:4)))) %>% formatCurrency(numeric_variables, "", digits = 0) %>% formatPercentage(percent_variables, 1)
  })
  
  output$plot_disability <- renderPlotly({create_summary_chart(d=census_data, p=input$Place, y=input$Year, v="Disability", val="share", f=100, dec=1, d.title="% of Total Population",s="%",d.clr="#00A7A0")})
  
  output$disability_map <- renderLeaflet({create_tract_map(t=census_data, v="Disabled", y=input$Year, d.clr="GnBu", p=input$Place, val="share", d.title="People with a Disability", dec=1, f=100, s="%")})
  
  # house section ----
  ## Housing Type, Home Values, Monthly Rental Cost and Home Ownership
  
  house_tab_server(id = "house", 
                   census_data = census_data, 
                   place = reactive(input$Place),
                   year = reactive(input$Year),
                   numeric_variables = numeric_variables, 
                   percent_variables = percent_variables)
  
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
  
  ## TIP Tab Panel Information ----
  
  output$tip_map <- renderLeaflet({create_tip_map(p=input$Place, plan.yr="2021-2024 TIP", d.title="Transportation Improvement Program")})
  
  output$table_tip <- DT::renderDataTable({
    datatable(create_project_table(p=input$Place,i=projects.shape,f=final.nms,plan.yr="2021-2024 TIP"), rownames = FALSE, options = list(pageLength = 10, columnDefs = list(list(className = 'dt-center', targets = 4:6)))) %>% formatCurrency(currency.rtp , "$", digits = 0)
  })
  
  ## RTP Tab Panel Information ----
  
  output$rtp_map <- renderLeaflet({create_rtp_map(p=input$Place, plan.yr=rtp.status, d.title="Regional Transportation Plan")})
  
  output$table_rtp <- DT::renderDataTable({
    datatable(create_project_table(p=input$Place,i=projects.shape,f=final.nms,plan.yr=rtp.status), rownames = FALSE, options = list(pageLength = 10, columnDefs = list(list(className = 'dt-center', targets = 4:6)))) %>% formatCurrency(currency.rtp , "$", digits = 0)
  })

  
})
