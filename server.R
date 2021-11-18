# Define server logic required to draw the map for the main panel
shinyServer(function(input, output) {
    
    output$general_heading <- renderText({
        paste(input$Place)
    })

    output$place_map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addPolygons(data = community.shape %>% filter(NAME %in% input$Place),
                        fillColor = "76787A",
                        weight = 4,
                        opacity = 1.0,
                        color = "#91268F",
                        dashArray = "4",
                        fillOpacity = 0.0)%>%
            setView(lng=find_place_data(input$Place,"INTPTLON"), lat=find_place_data(input$Place,"INTPTLAT"), zoom=find_place_data(input$Place,"ZOOM"))
    })
    
    output$place_pop_chart <- renderPlotly({create_line_chart(d=ofm.pop, p=input$Place, c="jurisdiction_name", w.x="estimate_year", w.y="total_population", w.title="Total Population")})
    
    output$Population <- renderText({
        paste("Population: ", return_estimate(census_data, input$Place, input$Year, v=c("DP02_0086","DP02_0087"),"estimate",-2))
    })
    
    output$MedianAge <- renderText({
        paste("Median Age: ", return_estimate(census_data, input$Place, input$Year, "DP05_0018","estimate",1), " years")
    })
    
    output$MedianIncome <- renderText({
        paste0("Median HH Income: $", return_estimate(census_data, input$Place, input$Year, "DP03_0062","estimate",-2))
    })
    
    output$AvgHHSize <- renderText({
        paste("Average HH Size: ", return_estimate(census_data, input$Place, input$Year, v=c("DP02_0015","DP02_0016"),"estimate",1), " people per household")
    })
    
    output$UnempRate <- renderText({
        paste0("Unemployment Rate: ", return_estimate(census_data, input$Place, input$Year, "DP03_0009","estimate_percent",1),"%")
    })
    
    output$AvgTT <- renderText({
        paste("Travel Time to Work: ", return_estimate(census_data, input$Place, input$Year, "DP03_0025","estimate",1), " minutes")
    })
    
    output$table_ms <- DT::renderDataTable({
        datatable(create_summary_table(census_data,input$Place,input$Year,"variable_name",ms_var,ms_cols,ms_total,ms_remove,ms_order),rownames = FALSE, options = list(pageLength = mode_length, columnDefs = list(list(className = 'dt-center', targets =1:4)))) %>% formatCurrency(numeric_ms, "", digits = 0) %>% formatPercentage(percent_ms, 1)
    })
    
    output$plot_ms <- renderPlotly({create_summary_chart(d=census_data, p=input$Place, y=input$Year, v="variable_name", d.var=ms_var, d.cols=ms_cols,
                                                                 d.tot=ms_total, d.rem=ms_remove, d.ord=ms_order, d.title="% of Total Commuters",
                                                                 d.clr="#91268F")})
    output$ms_heading <- renderText({
        paste("Mode to Work: ",  input$Place, " Residents")
    })
    
    output$table_tt <- DT::renderDataTable({
        datatable(create_summary_table(census_data,input$Place,input$Year,"variable_name",tt_var, tt_cols,tt_total,tt_remove,tt_order),rownames = FALSE, options = list(pageLength = tt_length, columnDefs = list(list(className = 'dt-center', targets =1:4)))) %>% formatCurrency(numeric_tt, "", digits = 0) %>% formatPercentage(percent_tt, 1)
    })
    
    output$plot_tt <- renderPlotly({create_summary_chart(d=census_data, p=input$Place, y=input$Year, v="variable_name", d.var=tt_var, d.cols=tt_cols,
                                                             d.tot=tt_total, d.rem=tt_remove, d.ord=tt_order, d.title="% of Total Commuters",
                                                             d.clr="#F05A28")})

    output$CensusBackground <- renderText({
        paste("As a State Data Center for the central Puget Sound region, PSRC keeps a complete inventory of data released from the 1990, 2000, and 2010 censuses, as well as the American Community Survey (ACS).  The American Community Survey (ACS) is a product of the U.S. Census Bureau. Cities and counties use the ACS to track the well-being of children, families, and the elderly. They use it to determine where to locate new roads and transit routes, schools, and hospitals. This portal includes demographic profiles which include age, sex, income, household size, education, and other topics for all cities and towns in the PSRC region.")
    })

    output$DemographicBackground <- renderText({
        paste("Most of the information on demographic characteristics is summarized in Data Profile 5 (DP05) with household, disability and ancestry data in Data Profile 2 (DP02). DP05 includes data on age and race and is a summarization of a variety of detailed tables related to age and race contained within the American Community Survey datasets.")
    }) 

    output$HousingBackground <- renderText({
        paste("Housing characteristics are summarized in Data Profile 4 (DP04) and includes data on occupancy, units, bedrooms, costs, tenure, value and vehicle availability.")
    }) 

    output$JobsBackground <- renderText({
        paste("Job and income characteristics are summarized in Data Profile 3 (DP03) and includes data on occupations, household income, health insurance and mode to work.")
    })     
        
    output$occupation_heading <- renderText({
        paste("Occupations for Residents in ",  input$Place)
    })

    output$industry_heading <- renderText({
        paste("Industry of Occupation for Residents in ",  input$Place)
    })    

    output$income_heading <- renderText({
        paste("Household Income for Residents in ",  input$Place)
    })     
            
    output$plot_occupation <- renderPlotly({create_summary_chart(d=census_data, p=input$Place, y=input$Year, v="variable_name", d.var=occ_var, d.cols=occ_cols,
                                                              d.tot=occ_total, d.rem=occ_remove, d.ord=occ_order, d.title="% of Total Workers",
                                                              d.clr="#91268F")})
    
    output$plot_industry <- renderPlotly({create_summary_chart(d=census_data, p=input$Place, y=input$Year, v="variable_name", d.var=ind_var, d.cols=ind_cols,
                                                                 d.tot=ind_total, d.rem=ind_remove, d.ord=ind_order, d.title="% of Total Workers",
                                                                 d.clr="#F05A28")})
    
    output$plot_income <- renderPlotly({create_summary_chart(d=census_data, p=input$Place, y=input$Year, v="variable_name", d.var=inc_var, d.cols=inc_cols,
                                                               d.tot=inc_total, d.rem=inc_remove, d.ord=inc_order, d.title="% of Total Workers",
                                                               d.clr="#8CC63E")})

    output$table_occupation <- DT::renderDataTable({
        datatable(create_summary_table(census_data,input$Place,input$Year,"variable_name",occ_var,occ_cols,occ_total,occ_remove,occ_order),rownames = FALSE, options = list(pageLength = job_length, columnDefs = list(list(className = 'dt-center', targets =1:4)))) %>% formatCurrency(numeric_jobs, "", digits = 0) %>% formatPercentage(percent_jobs, 1)
    })
    
    output$table_industry <- DT::renderDataTable({
        datatable(create_summary_table(census_data,input$Place,input$Year,"variable_name",ind_var,ind_cols,ind_total,ind_remove,ind_order),rownames = FALSE, options = list(pageLength = job_length, columnDefs = list(list(className = 'dt-center', targets =1:4)))) %>% formatCurrency(numeric_jobs, "", digits = 0) %>% formatPercentage(percent_jobs, 1)
    })
    
    output$table_income <- DT::renderDataTable({
        datatable(create_summary_table(census_data,input$Place,input$Year,"variable_name",inc_var,inc_cols,inc_total,inc_remove,inc_order),rownames = FALSE, options = list(pageLength = job_length, columnDefs = list(list(className = 'dt-center', targets =1:4)))) %>% formatCurrency(numeric_jobs, "", digits = 0) %>% formatPercentage(percent_jobs, 1)
    })
    
    output$table_housing <- DT::renderDataTable({
        datatable(create_summary_table(census_data,input$Place,input$Year,"variable_name",hs_var, hs_cols,hs_total,hs_remove,hs_order),rownames = FALSE, options = list(pageLength = house_length, columnDefs = list(list(className = 'dt-center', targets =1:4)))) %>% formatCurrency(numeric_hs, "", digits = 0) %>% formatPercentage(percent_hs, 1)
    })
    
    output$plot_housing <- renderPlotly({create_summary_chart(d=census_data, p=input$Place, y=input$Year, v="variable_name", d.var=hs_var, d.cols=hs_cols,
                                                               d.tot=hs_total, d.rem=hs_remove, d.ord=hs_order, d.title="% of Total Housing Units",
                                                               d.clr="#F05A28")})
    
    output$table_vehicles <- DT::renderDataTable({
        datatable(create_summary_table(census_data,input$Place,input$Year,"variable_name",va_var, va_cols,va_total,va_remove,va_order),rownames = FALSE, options = list(pageLength = va_length, columnDefs = list(list(className = 'dt-center', targets =1:4)))) %>% formatCurrency(numeric_va, "", digits = 0) %>% formatPercentage(percent_va, 1)
    })
    
    output$plot_vehicles <- renderPlotly({create_summary_chart(d=census_data, p=input$Place, y=input$Year, v="variable_name", d.var=va_var, d.cols=va_cols,
                                                                d.tot=va_total, d.rem=va_remove, d.ord=va_order, d.title="% of Total Occupied Units",
                                                                d.clr="#00A7A0")})
    
    output$table_homevalue <- DT::renderDataTable({
        datatable(create_summary_table(census_data,input$Place,input$Year,"variable_name",hv_var, hv_cols,hv_total,hv_remove,hv_order),rownames = FALSE, options = list(pageLength = hv_length, columnDefs = list(list(className = 'dt-center', targets =1:4)))) %>% formatCurrency(numeric_hv, "", digits = 0) %>% formatPercentage(percent_hv, 1)
    })
    
    output$plot_homevalue <- renderPlotly({create_summary_chart(d=census_data, p=input$Place, y=input$Year, v="variable_name", d.var=hv_var, d.cols=hv_cols,
                                                                d.tot=hv_total, d.rem=hv_remove, d.ord=hv_order, d.title="% of Owner Occupied Units",
                                                                d.clr="#F05A28")})
    
    output$table_monthlyrent <- DT::renderDataTable({
        datatable(create_summary_table(census_data,input$Place,input$Year,"variable_name",rent_var, rent_cols,rent_total,rent_remove,rent_order),rownames = FALSE, options = list(pageLength = rent_length, columnDefs = list(list(className = 'dt-center', targets =1:4)))) %>% formatCurrency(numeric_rent, "", digits = 0) %>% formatPercentage(percent_rent, 1)
    })
    
    output$plot_monthlyrent <- renderPlotly({create_summary_chart(d=census_data, p=input$Place, y=input$Year, v="variable_name", d.var=rent_var, d.cols=rent_cols,
                                                                  d.tot=rent_total, d.rem=rent_remove, d.ord=rent_order, d.title="% of Rental Occupied Units",
                                                                  d.clr="#8CC63E")})
 
    output$table_age <- DT::renderDataTable({
        datatable(create_summary_table(census_data,input$Place,input$Year,"variable_name",age_var,age_cols,age_total,age_remove,age_order),rownames = FALSE, options = list(pageLength = age_length, columnDefs = list(list(className = 'dt-center', targets =1:4)))) %>% formatCurrency(numeric_age, "", digits = 0) %>% formatPercentage(percent_age, 1)
    })
    
    output$plot_age <- renderPlotly({create_summary_chart(d=census_data, p=input$Place, y=input$Year, v="variable_name", d.var=age_var, d.cols=age_cols,
                                                          d.tot=age_total, d.rem=age_remove, d.ord=age_order, d.title="% of Total Population",
                                                          d.clr="#91268F")})
    
    output$downloadData <- downloadHandler(
        filename = function() {paste0("acs-5yr-profile-",as.character(as.integer(2019)),"-pl-",str_replace("Bainbridge Island"," ","_"),".xlsx")},
        content <- function(file) {file.copy(here(paste0("data-profiles/acs-5yr-profile-",as.character(as.integer(2019)),"-pl-",str_replace("Bainbridge Island"," ","_"),".xlsx")),file)},
        contentType = "application/Excel"
    )

    output$table_race <- DT::renderDataTable({
        datatable(create_summary_table(census_data,input$Place,input$Year,"variable_name",race_var,race_cols,race_total,race_remove,race_order),rownames = FALSE, options = list(pageLength = race_length, columnDefs = list(list(className = 'dt-center', targets =1:4)))) %>% formatCurrency(numeric_race, "", digits = 0) %>% formatPercentage(percent_race, 1)
    })
    
    output$plot_race <- renderPlotly({create_summary_chart(d=census_data, p=input$Place, y=input$Year, v="variable_name", d.var=race_var, d.cols=race_cols,
                                                           d.tot=race_total, d.rem=race_remove, d.ord=race_order, d.title="% of Total Population",
                                                           d.clr="#8CC63E")})

    output$monthlyrent_map <- renderLeaflet({create_tract_map_pick_variable(census_data, "DP04_0134",input$Year, "Greens", input$Place, "estimate", "variable_name", "Median Rent", "$","")})
    
    output$homevalue_map <- renderLeaflet({create_tract_map_pick_variable(census_data, "DP04_0089",input$Year, "Oranges", input$Place, "estimate", "variable_name", "Median Home Value", "$","")})
    
    output$zerocar_map <- renderLeaflet({create_tract_map_pick_variable(census_data, "DP04_0058",input$Year, "Blues", input$Place, "estimate_percent", "variable_name", "% Zero Car HH's", "","%")})
    
    output$traveltime_map <- renderLeaflet({create_tract_map_pick_variable(census_data, "DP03_0025",input$Year, "Oranges", input$Place, "estimate", "variable_name", "Time to Work (minutes)", "","")})
    
    output$disability_map <- renderLeaflet({create_tract_map_pick_variable(census_data, v="With a disability", input$Year, "Oranges", input$Place, "estimate_percent", "variable_description", "Share of People with a Disability", "","%")})
    
    output$modeshare_map <- renderLeaflet({create_tract_map_pick_variable(census_data, input$Mode, input$Year, "Purples", input$Place, "estimate_percent", "variable_description", "Share", "","%")})

    output$race_map <- renderLeaflet({create_tract_map_pick_variable(t=census_data, v=input$Race, y=input$Year, d.clr="Greens", p=input$Place, e="estimate_percent", 
                                                                     v.type="variable_description", w.title="Share of Population", w.pre="", w.suff="%")})    

    
    output$age_map <- renderLeaflet({create_tract_map_pick_variable(t=census_data, v="Median age (years)", y=input$Year, d.clr="Purples", p=input$Place, e="estimate", 
                                                                     v.type="variable_description", w.title="Median Age", w.pre="", w.suff="")})    
    
    output$housingunits_map <- renderLeaflet({create_tract_map_pick_variable(t=census_data, v=input$HU, y=input$Year, d.clr="Oranges", p=input$Place, e="estimate_percent", 
                                                                     v.type="variable_description", w.title="Share of Housing Units", w.pre="", w.suff="%")})    
    
    output$medianincome_map <- renderLeaflet({create_tract_map_pick_variable(t=census_data, v="Median household income (dollars)", y=input$Year, d.clr="Greens", p=input$Place, e="estimate", 
                                                                             v.type="variable_description", w.title="Median Household Income", w.pre="$", w.suff="")})    

    output$occupation_map <- renderLeaflet({create_tract_map_pick_variable(t=census_data, v=input$OCC, y=input$Year, d.clr="Purples", p=input$Place, e="estimate_percent", 
                                                                             v.type="variable_description", w.title="Share of Total Workers", w.pre="", w.suff="%")})    
 
    output$industry_map <- renderLeaflet({create_tract_map_pick_variable(t=census_data, v=input$IND, y=input$Year, d.clr="Oranges", p=input$Place, e="estimate_percent", 
                                                                           v.type="variable_description", w.title="Share of Total Workers", w.pre="", w.suff="%")})    
    
    output$rtp_heading <- renderText({
        paste("Regional Capacity Project List: Projects in ",  input$Place)
    }) 
    
    output$tip_heading <- renderText({
        paste("Transportation Improvement Program: Projects in ",  input$Place)
    })
    
    output$tip_map <- renderLeaflet({create_tip_map(p=input$Place)})
    
    output$rtp_map <- renderLeaflet({create_project_map(p=input$Place)})
    
    output$table_rtp <- DT::renderDataTable({
        datatable(create_project_table(p=input$Place,i=rtp.shape,o=rtp.cols,f=final.nms), rownames = FALSE, options = list(pageLength = proj.length, columnDefs = list(list(className = 'dt-center', targets = 4:6)))) %>% formatCurrency(currency.rtp , "$", digits = 0)
    })
    
    output$table_tip <- DT::renderDataTable({
        datatable(create_project_table(p=input$Place,i=tip.shape,o=tip.cols,f=final.nms), rownames = FALSE, options = list(pageLength = proj.length, columnDefs = list(list(className = 'dt-center', targets = 4:6)))) %>% formatCurrency(currency.rtp , "$", digits = 0)
    })
    
    output$place_rgeo <- renderText({
        paste("Regional Geography:",find_rgeo_data(p=input$Place, v="regional_geography"))
    }) 
    
    output$place_airaff <- renderText({
        paste("Aiport Affected Community:", find_rgeo_data(p=input$Place, v="airport_affected"))
    }) 
    
    observeEvent(input$showpanel, {
        
        if(input$showpanel == TRUE) {
            removeCssClass("Main", "col-sm-12")
            addCssClass("Main", "col-sm-8")
            shinyjs::show(id = "sidebar")
            shinyjs::enable(id = "sidebar")
        }
        else {
            removeCssClass("Main", "col-sm-8")
            addCssClass("Main", "col-sm-12")
            shinyjs::hide(id = "sidebar")
        }
    })
    
    
})
