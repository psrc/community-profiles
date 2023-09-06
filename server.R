# Define server logic required to draw the map for the main panel
shinyServer(function(input, output, session) {
  
  ## RDI ----
  
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
    updateTabsetPanel(session, inputId = 'tab_housing', selected = 'units')
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
  
  
  ## Housing Type Tab Panel Information ----
  
  output$table_housingtype <- DT::renderDataTable({
    datatable(create_summary_table(t=census_data,p=input$Place,y=input$Year,v="Housing Type"),rownames = FALSE, options = list(pageLength = 15, columnDefs = list(list(className = 'dt-center', targets =1:4)))) %>% formatCurrency(numeric_variables, "", digits = 0) %>% formatPercentage(percent_variables, 1)
  })
  
  output$plot_housingtype <- renderPlotly({create_summary_chart(d=census_data, p=input$Place, y=input$Year, v="Housing Type", val="share", f=100, dec=1, d.title="% of Total Housing Units",s="%",d.clr="#91268F")})
  
  output$housingtype_map <- renderLeaflet({create_tract_map(t=census_data, v="Middle-Housing", y=input$Year, d.clr="Purples", p=input$Place, val="share", d.title="Medium Density Housing", dec=1, f=100, s="%")})
  
  ## Home Value Tab Panel Information ----
  
  output$table_homevalue <- DT::renderDataTable({
    datatable(create_summary_table(t=census_data,p=input$Place,y=input$Year,v="Home Value"),rownames = FALSE, options = list(pageLength = 15, columnDefs = list(list(className = 'dt-center', targets =1:4)))) %>% formatCurrency(numeric_variables, "", digits = 0) %>% formatPercentage(percent_variables, 1)
  })
  
  output$plot_homevalue <- renderPlotly({create_summary_chart(d=census_data, p=input$Place, y=input$Year, v="Home Value", val="share", f=100, dec=1, d.title="% of Total Ownership Units",s="%",d.clr="#8CC63E")})
  
  output$homevalue_map <- renderLeaflet({create_tract_map(t=census_data, v="Home-Value", y=input$Year, d.clr="Greens", p=input$Place, val="estimate", d.title="Median Value", dec=0, f=1, s="", pre="$")})
  
  ## Monthly Rent Tab Panel Information ----
  
  output$table_monthlyrent <- DT::renderDataTable({
    datatable(create_summary_table(t=census_data,p=input$Place,y=input$Year,v="Monthly Rent"),rownames = FALSE, options = list(pageLength = 15, columnDefs = list(list(className = 'dt-center', targets =1:4)))) %>% formatCurrency(numeric_variables, "", digits = 0) %>% formatPercentage(percent_variables, 1)
  })
  
  output$plot_monthlyrent <- renderPlotly({create_summary_chart(d=census_data, p=input$Place, y=input$Year, v="Monthly Rent", val="share", f=100, dec=1, d.title="% of Total Rental Units",s="%",d.clr="#F05A28")})
  
  output$monthlyrent_map <- renderLeaflet({create_tract_map(t=census_data, v="Rent", y=input$Year, d.clr="Oranges", p=input$Place, val="estimate", d.title="Median Rent", dec=0, f=1, s="", pre="$")})
  
  ## Home Ownership Tab Panel Information ----
  
  output$table_ownership <- DT::renderDataTable({
    datatable(create_summary_table(t=census_data,p=input$Place,y=input$Year,v="Home Ownership"),rownames = FALSE, options = list(pageLength = 15, columnDefs = list(list(className = 'dt-center', targets =1:4)))) %>% formatCurrency(numeric_variables, "", digits = 0) %>% formatPercentage(percent_variables, 1)
  })
  
  output$plot_ownership <- renderPlotly({create_summary_chart(d=census_data, p=input$Place, y=input$Year, v="Home Ownership", val="share", f=100, dec=1, d.title="% of Total Households",s="%",d.clr="#00A7A0")})
  
  output$ownership_map <- renderLeaflet({create_tract_map(t=census_data, v="Home-Owner", y=input$Year, d.clr="GnBu", p=input$Place, val="share", d.title="% Home Ownership", dec=1, f=100, s="%", pre="")})
  
  ## Educational Attainment Tab Panel Information ----
  
  output$table_edu <- DT::renderDataTable({
    datatable(create_summary_table(t=census_data,p=input$Place,y=input$Year,v="Educational Attainment"),rownames = FALSE, options = list(pageLength = 15, columnDefs = list(list(className = 'dt-center', targets =1:4)))) %>% formatCurrency(numeric_variables, "", digits = 0) %>% formatPercentage(percent_variables, 1)
  })
  
  output$plot_edu <- renderPlotly({create_summary_chart(d=census_data, p=input$Place, y=input$Year, v="Educational Attainment", val="share", f=100, dec=1, d.title="% of Total Population",s="%",d.clr="#91268F")})
  
  output$edu_map <- renderLeaflet({create_tract_map(t=census_data, v="College-Degree", y=input$Year, d.clr="Purples", p=input$Place, val="share", d.title="College Degree", dec=1, f=100, s="%")})
  
  ## Occupation Tab Panel Information ----
  
  output$table_occupation <- DT::renderDataTable({
    datatable(create_summary_table(t=census_data,p=input$Place,y=input$Year,v="Occupation"),rownames = FALSE, options = list(pageLength = 15, columnDefs = list(list(className = 'dt-center', targets =1:4)))) %>% formatCurrency(numeric_variables, "", digits = 0) %>% formatPercentage(percent_variables, 1)
  })
  
  output$plot_occupation <- renderPlotly({create_summary_chart(d=census_data, p=input$Place, y=input$Year, v="Occupation", val="share", f=100, dec=1, d.title="% of Total Workers",s="%",d.clr="#8CC63E")})
  
  output$occupation_map <- renderLeaflet({create_tract_map(t=census_data, v="Office", y=input$Year, d.clr="Greens", p=input$Place, val="share", d.title="% Office Workers", dec=1, f=100, s="%", pre="")})
  
  ## Industry Tab Panel Information ----
  
  output$table_industry <- DT::renderDataTable({
    datatable(create_summary_table(t=census_data,p=input$Place,y=input$Year,v="Industry"),rownames = FALSE, options = list(pageLength = 15, columnDefs = list(list(className = 'dt-center', targets =1:4)))) %>% formatCurrency(numeric_variables, "", digits = 0) %>% formatPercentage(percent_variables, 1)
  })
  
  output$plot_industry <- renderPlotly({create_summary_chart(d=census_data, p=input$Place, y=input$Year, v="Industry", val="share", f=100, dec=1, d.title="% of Total Workers",s="%",d.clr="#F05A28")})
  
  output$industry_map <- renderLeaflet({create_tract_map(t=census_data, v="Retail-Accomodations", y=input$Year, d.clr="Oranges", p=input$Place, val="share", d.title="% Retail-Accom. Workers", dec=1, f=100, s="%", pre="")})
  
  ## Income Tab Panel Information ----
  
  output$table_income <- DT::renderDataTable({
    datatable(create_summary_table(t=census_data,p=input$Place,y=input$Year,v="Household Income"),rownames = FALSE, options = list(pageLength = 15, columnDefs = list(list(className = 'dt-center', targets =1:4)))) %>% formatCurrency(numeric_variables, "", digits = 0) %>% formatPercentage(percent_variables, 1)
  })
  
  output$plot_income <- renderPlotly({create_summary_chart(d=census_data, p=input$Place, y=input$Year, v="Household Income", val="share", f=100, dec=1, d.title="% of Total Households",s="%",d.clr="#00A7A0")})
  
  output$income_map <- renderLeaflet({create_tract_map(t=census_data, v="Income", y=input$Year, d.clr="GnBu", p=input$Place, val="estimate", d.title="Median HH Income", dec=0, f=1, s="", pre="$")})
  
  ## Mode Share Tab Panel Information ----
  
  output$table_modes <- DT::renderDataTable({
    datatable(create_summary_table(t=census_data,p=input$Place,y=input$Year,v="Mode Share"),rownames = FALSE, options = list(pageLength = 15, columnDefs = list(list(className = 'dt-center', targets =1:4)))) %>% formatCurrency(numeric_variables, "", digits = 0) %>% formatPercentage(percent_variables, 1)
  })
  
  output$plot_modes <- renderPlotly({create_summary_chart(d=census_data, p=input$Place, y=input$Year, v="Mode Share", val="share", f=100, dec=1, d.title="% of Total Workers",s="%",d.clr="#91268F")})
  
  output$modes_map <- renderLeaflet({create_tract_map(t=census_data, v="Non-Vehicle", y=input$Year, d.clr="Purples", p=input$Place, val="share", d.title="Non-Vehicle Modes to Work", dec=0, f=100, s="%", pre="")})
  
  ## Travel Time Tab Panel Information ----
  
  output$table_time <- DT::renderDataTable({
    datatable(create_summary_table(t=census_data,p=input$Place,y=input$Year,v="Travel Time"),rownames = FALSE, options = list(pageLength = 15, columnDefs = list(list(className = 'dt-center', targets =1:4)))) %>% formatCurrency(numeric_variables, "", digits = 0) %>% formatPercentage(percent_variables, 1)
  })
  
  output$plot_time <- renderPlotly({create_summary_chart(d=census_data, p=input$Place, y=input$Year, v="Travel Time", val="share", f=100, dec=1, d.title="% of Total Workers",s="%",d.clr="#8CC63E")})
  
  output$time_map <- renderLeaflet({create_tract_map(t=census_data, v="Time", y=input$Year, d.clr="Greens", p=input$Place, val="estimate", d.title="Travel Time to Work", dec=1, f=1, s="", pre="")})
  
  ## Travel Departure Time Tab Panel Information ----
  
  output$table_depart <- DT::renderDataTable({
    datatable(create_summary_table(t=census_data,p=input$Place,y=input$Year,v="Departure Time"),rownames = FALSE, options = list(pageLength = 15, columnDefs = list(list(className = 'dt-center', targets =1:4)))) %>% formatCurrency(numeric_variables, "", digits = 0) %>% formatPercentage(percent_variables, 1)
  })
  
  output$plot_depart <- renderPlotly({create_summary_chart(d=census_data, p=input$Place, y=input$Year, v="Departure Time", val="share", f=100, dec=1, d.title="% of Total Workers",s="%",d.clr="#F05A28")})
  
  output$depart_map <- renderLeaflet({create_tract_map(t=census_data, v="AM-Peak", y=input$Year, d.clr="Oranges", p=input$Place, val="share", d.title="AM Peak Departure", dec=0, f=100, s="%", pre="")})
  
  ## Vehicle Availability Tab Panel Information ----
  
  output$table_vehicles <- DT::renderDataTable({
    datatable(create_summary_table(t=census_data,p=input$Place,y=input$Year,v="Vehicle Availability"),rownames = FALSE, options = list(pageLength = 15, columnDefs = list(list(className = 'dt-center', targets =1:4)))) %>% formatCurrency(numeric_variables, "", digits = 0) %>% formatPercentage(percent_variables, 1)
  })
  
  output$plot_vehicles <- renderPlotly({create_summary_chart(d=census_data, p=input$Place, y=input$Year, v="Vehicle Availability", val="share", f=100, dec=1, d.title="% of Total Households",s="%",d.clr="#00A7A0")})
  
  output$vehicles_map <- renderLeaflet({create_tract_map(t=census_data, v="Zero-Car", y=input$Year, d.clr="GnBu", p=input$Place, val="share", d.title="Zero Car HH's", dec=1, f=100, s="%", pre="")})
  
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
