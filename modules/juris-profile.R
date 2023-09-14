# Display jurisdiction profile (on sidebar)

juris_profile_ui <- function(id) {
  ns <- NS(id)
  
  div(
    uiOutput(ns("Population_ui")),
    uiOutput(ns("POCShare_ui")),
    uiOutput(ns("MedianAge_ui")),
    uiOutput(ns("DisabledShare_ui")),
    uiOutput(ns("MedianIncome_ui")),
    uiOutput(ns("AvgHHSize_ui")),
    uiOutput(ns("OwnShare_ui")),
    uiOutput(ns("UnempRate_ui")),
    uiOutput(ns("AvgTT_ui")),
    h3("Regional Definitions:"),
    uiOutput(ns("place_rgeo_ui")),
    uiOutput(ns("place_airaff_ui")),
    style = "margin-bottom: 1rem;"
  )
  
}

juris_profile_server <- function(id, place, year) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    output$Population_ui <- renderUI({

      t <- return_estimate(t = census_data,
                      p = place(),
                      y = year(),
                      v = "Population",
                      val = "estimate",
                      d = -2)
      cat <- div("Population", style = "font-weight: bold;")
      div(class = "profile", cat,": ", t)
    })

    output$MedianAge_ui <- renderUI({
      t <- return_estimate(t = census_data, 
                           p = place(), 
                           y = year(), 
                           v = "Median Age",
                           val = "estimate",
                           d = 1)
      cat <- div("Median Age", style = "font-weight: bold;")
      div(class = "profile", cat,": ", t, " years")
    })

    output$MedianIncome_ui <- renderUI({
      t <- return_estimate(t = census_data, 
                           p = place(), 
                           y = year(), 
                           v = "Median HH Income",
                           "estimate",
                           -2)
      cat <- div("Median HH Income", style = "font-weight: bold;")
      div(class = "profile", cat," : $", t)
    })

    output$AvgHHSize_ui <- renderUI({
      t <- return_estimate(t = census_data, 
                           p = place(), 
                           y = year(), 
                           v = "Avg HH Size",
                           val = "estimate",
                           d = 2)
      cat <- div("HH Size", style = "font-weight: bold;")
      div(class = "profile", cat, ": ", t, " people per household")
    })

    output$UnempRate_ui <- renderUI({
      t <- return_estimate(t = census_data, 
                           p = place(), 
                           y = year(), 
                           v = "Unemployment Rate",
                           val = "estimate_percent",
                           d = 1)
      cat <- div("Unemployment Rate", style = "font-weight: bold;")
      div(class = "profile", cat, ": ", t ,"%")
    })

    output$AvgTT_ui <- renderUI({
      t <- return_estimate(t = census_data, 
                           p = place(), 
                           y = year(), 
                           v = "Avg Travel Time to Work",
                           val = "estimate",
                           d = 1)
      cat <- div("Travel Time to Work", style = "font-weight: bold;")
      div(class = "profile", cat,": ", t, " minutes")
    })

    output$DisabledShare_ui <- renderUI({
      t <- return_estimate(t = census_data, 
                           p = place(), 
                           y = year(), 
                           v = "With A Disability",
                           val = "share",
                           d = 1)
      cat <- div("People with a Disability", style = "font-weight: bold;")
      div(class = "profile", cat, ": ", t,"%")
    })

    output$OwnShare_ui <- renderUI({
      t <- return_estimate(t = census_data, 
                           p = place(), 
                           y = year(), 
                           v = "Own",
                           val = "share",
                           d = 0)
      cat <- div("Home Ownership", style = "font-weight: bold;")
      div(class = "profile", cat, ": ", t, "%")
    })

    output$POCShare_ui <- renderUI({
      t <- return_estimate(t = census_data, 
                           p = place(), 
                           y = year(), 
                           v = "Non-Hispanic White",
                           "share",
                           1)
      cat <- div("People of Color", style = "font-weight: bold;") 
      div(class = "profile", cat, ": ", as.character((100-as.numeric(t))), "%")
    })

    output$place_rgeo_ui <- renderUI({
      t <- find_rgeo_data(p = place(), 
                          v="regional_geography")
      cat <- div("Regional Geography", style = "font-weight: bold;")
      div(class = "profile", cat, ":", t)
    })

    output$place_airaff_ui <- renderUI({
      t <- find_rgeo_data(p = place(), 
                          v = "airport_affected")
      cat <- div("Airport Affected Community", style = "font-weight: bold;")
      div(class = "profile", cat, ":", t)
    })

    
  }) # end moduleServer
  
}