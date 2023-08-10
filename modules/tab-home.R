# Display UI for RDI tab

home_tab_ui <- function(id) {
  ns <- NS(id)
  
  intro <- p("As a State Data Center for the central Puget Sound region, PSRC keeps a complete inventory of data released from the U.S. Census Bureau. 
  Cities and counties use it to track the well-being of children, families, and the elderly as well as to determine where to locate new public facilities. 
  This portal includes demographic profiles on a variety of topics for all cities and towns in the PSRC region.")
  
  body <- p("The data  on this dashboard is a snapshot of the information that is available from the US Census Bureau. 
  In order to view data for all the cities and towns in the Puget Sound region, we are limited to using American Community Survey (ACS) 5 year data. 
  For this reason, you will only find two non-overlapping sets of data on this dashboard. 
  By only including non-overlapping data, users can analyze the data for changing trends.
  The data on this dashboard is divided into six categories:")
  
  note <- p("*Data measures to support analysis of racially disparate impacts are pulled from HUD’s
                        Comprehensive Housing Affordability Strategy (CHAS) dataset.", style = "font-size: 10pt;")
  
  tabPanel(icon("city"),
           h1("Community Profiles"),
           intro,
           fluidRow(
             column(width=7,
                    body,
                    icon("users"),"People Measures", br(),
                    icon("home"),"Household & Housing Measures", br(),
                    HTML(paste("<b>", "RDI", "</b>")), "Racially Disparate Impacts Measures*", br(),
                    icon("briefcase"),"Job & Income Measures", br(),
                    icon("car"),"Transportation Measures", br(),
                    icon("wrench"),"Transportation Projects", br(),br(),
                    note
             ),
             column(width=5, leafletOutput(ns("place_map")))
             
           ), # end fluidRow
           hr()
           
  ) # end tabPanel
  
  
  
}

home_tab_server <- function(id, place) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    ## This section is for all the data on the Main Overview Page
    
    # output$general_heading <- renderText({
    #   paste(input$Place)
    # })
    
    output$place_map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = community.shape %>% filter(geog_name %in% place()),
                    fillColor = "76787A",
                    weight = 4,
                    opacity = 1.0,
                    color = "#91268F",
                    dashArray = "4",
                    fillOpacity = 0.0)
    })
    
    
  }) # end moduleServer
  
}