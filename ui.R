# User Interface for a Place Selection with a map returned for that place.

shinyUI(
    fluidPage(sidebarLayout(
        sidebarPanel(id = "sidebar",
            div(img(src="psrc-logo.png", width = 260, height = 92, style = "padding-top: 25px")),
            br(),
            selectInput("Place","Please Select the community you are interested in:",data_places, selected = "Bellevue"),
            selectInput("Year","Please Select the year you are interested in:",data_years, selected = 2019),
            textOutput("Population"),
            textOutput("MedianAge"),
            textOutput("MedianIncome"),
            textOutput("AvgHHSize"),
            textOutput("UnempRate"),
            textOutput("AvgTT"),
            h3("Note on Census Data:"),
            textOutput("CensusBackground"),
            br(),
            downloadLink('downloadData', label = "Download Data Profiles in Excel"),
            width=3),
        mainPanel(shinyjs::useShinyjs(), id ="Main",
                  bsButton("showpanel", "Show/hide sidebar", type = "toggle", value = TRUE),
            navbarPage(title = "", theme = "styles.css", windowTitle = "PSRC Community Profiles",
                             tabPanel(icon("city"),
                                      h1("Community Profiles"),
                                      "We invite you to explore our various data sets through our Community Profiles Data Portal. This data portal provides access to Census data, Regional Transportation Plan Projects and the Transportation Improvement Program and project related information by jurisdiction. If you can't find what you're looking for, or would like further information about Census or project related data products, please contact us and we will be happy to assist you.",
                                      hr(),
                                      h2(textOutput("general_heading")),
                                      hr(),
                                      leafletOutput("place_map"),
                                      hr()
                            ), # end of Overview tabset panel
            
                            tabPanel(icon("users"),
                                textOutput("DemographicBackground"),
                                tabsetPanel(
                                    
                                    tabPanel("Age",
                                             fluidRow(
                                                 column(width = 6, br(), br(), plotlyOutput("plot_age")),
                                                 column(width = 6, h2("Median Age"),leafletOutput("age_map"))
                                             ), # end of fluid row
                                             fluidRow(
                                                 column(width = 12,hr(),DT::dataTableOutput("table_age"))
                                             ) # end of fluid Row
                                    ), # end of age tab panel
                                          
                                    tabPanel("Race",
                                        fluidRow(
                                            column(width = 6, br(), br(), plotlyOutput("plot_race")),
                                            column(width = 6, selectInput("Race","",data_race, selected = "Black or African American"), leafletOutput("race_map"))
                                        ), # end of fluid row
                                        fluidRow(
                                            column(width = 12,hr(),DT::dataTableOutput("table_race"))
                                        ) # end of fluid Row
                                    ), # end of race tab panel
                                    
                                    tabPanel("Disability",
                                             fluidRow(
                                                 column(width = 12,h2("People with a Disability"),leafletOutput("disability_map"))
                                             ) # end of fluid row
                                    ) # end of Disability Tab Panel
                                          
                                ) # end of Demographics tabset panel
                            ), # end of Demographics Tab Panel
            
                            tabPanel(icon("home"),
                                textOutput("HousingBackground"),
                                tabsetPanel(
                                
                                    tabPanel("Housing Units",
                                             fluidRow(
                                                 column(width = 6, br(), br(), plotlyOutput("plot_housing")),
                                                 column(width = 6, selectInput("HU","",data_hu, selected = "20+ Units"), leafletOutput("housingunits_map"))
                                             ), # end of fluid row
                                             fluidRow(
                                                 column(width = 12,hr(),DT::dataTableOutput("table_housing"))
                                             ) # end of fluid Row
                                    ), # end of units tab panel
                                    
                                    tabPanel("Home Value",
                                        fluidRow(
                                            column(width = 6,
                                             verticalLayout(plotlyOutput("plot_homevalue"),
                                                            DT::dataTableOutput("table_homevalue"))),
                                            column(width = 6, h2("Median Home Value"), leafletOutput("homevalue_map",height="600px"))
                                        ) # end of Fluid Row
                                    ), # end of Home Value tab panel
                         
                                    tabPanel("Monthly Rent",
                                        fluidRow(
                                            column(width = 6,
                                             verticalLayout(plotlyOutput("plot_monthlyrent"),
                                                            DT::dataTableOutput("table_monthlyrent"))),
                                            column(width = 6, h2("Median Monthly Rent"), leafletOutput("monthlyrent_map",height="600px"))
                                        ) # end of Fluid Row
                                    ), # end of Monthly Rent tab panel
                         
                                    tabPanel("Vehicles Available",
                                        fluidRow(
                                            column(width = 6,
                                             verticalLayout(plotlyOutput("plot_vehicles"),
                                                            DT::dataTableOutput("table_vehicles"))),
                                            column(width = 6, h2("Zero Car Households"), leafletOutput("zerocar_map",height="600px"))
                                        ) # end of Fluid Row
                                    ) # end of Vehicle Availability tab panel 
                         
                                ) # end of Housing tabset panel
                            ), # end of Housing Tab Panel

                            tabPanel(icon("briefcase"),
                                     textOutput("JobsBackground"),
                                     tabsetPanel(
                                         
                                         tabPanel("Occupation",
                                                  fluidRow(
                                                      column(width = 6, br(), br(), plotlyOutput("plot_occupation")),
                                                      column(width = 6, selectInput("OCC","",data_occ, selected = "Service occupations"), leafletOutput("occupation_map"))
                                                  ), # end of fluid row
                                                  fluidRow(
                                                      column(width = 12,hr(),DT::dataTableOutput("table_occupation"))
                                                  ) # end of fluid Row
                                         ), # end of occupation tab panel
                                         
                                         tabPanel("Industy",
                                                  fluidRow(
                                                      column(width = 6, br(), br(), plotlyOutput("plot_industry")),
                                                      column(width = 6, selectInput("IND","",data_ind, selected = "Retail trade"), leafletOutput("industry_map"))
                                                  ), # end of fluid row
                                                  fluidRow(
                                                      column(width = 12,hr(),DT::dataTableOutput("table_industry"))
                                                  ) # end of fluid Row
                                         ), # end of industry tab panel
                                         
                                         tabPanel("Income",
                                                  fluidRow(
                                                      column(width = 6, br(), br(), plotlyOutput("plot_income")),
                                                      column(width = 6, h2("Median Household Income"), leafletOutput("medianincome_map"))
                                                  ), # end of fluid row
                                                  fluidRow(
                                                      column(width = 12,hr(),DT::dataTableOutput("table_income"))
                                                  ) # end of fluid Row
                                         ) # end of income tab panel
                                         
                                     ) # end of jobs and income tabset panel
                            ), # end of jobs and income Tab Panel
                            
                            tabPanel(icon("car"),
                                     
                                tabsetPanel(
                                        tabPanel("Mode Share",
                                                fluidRow(
                                                        column(width = 6,
                                                            verticalLayout(plotlyOutput("plot_ms"),
                                                                            DT::dataTableOutput("table_ms"))),
                                                        column(width = 6, selectInput("Mode","",data_modes), leafletOutput("modeshare_map",height="600px"))
                                                ) # end of Fluid Row
                                        ), # end of mode share tab panel
                                                 
                                        tabPanel("Travel Time to Work",
                                                fluidRow(
                                                        column(width = 6,
                                                            verticalLayout(plotlyOutput("plot_tt"),
                                                                            DT::dataTableOutput("table_tt"))),
                                                        column(width = 6, h2("Average Travel Time to Work"), leafletOutput("traveltime_map",height="600px"))
                                                ) # end of Fluid Row
                                        ) # end of travel time tab panel
                                                 
                                     ) # end of Transportation TabSet
                                     
                            ), # end of Transportation tabPanel

                            tabPanel(icon("info-circle"),
                                     h1("Data Sources"),
                                     "The data in this portal comes from a few key sources:",
                                     hr(),
                                     h2("Census Data"),
                                     "The Census Data used in this portal is stored in PSRC's central database but is available from the US Census Bureau. All tables can be downloaded either via the Census API (https://www.census.gov/data/developers/data-sets/acs-5year.html) or the Census Data page (https://data.census.gov/cedsci/).",
                                     br(),
                                     h3("Census Tables:"),
                                     "Travel Time to Work: Table B08303",
                                     br(),
                                     "Age: Data Profile 5 (DP05)",
                                     br(),
                                     "Disability: Data Profile 2 (DP02)",
                                     br(),
                                     "Housing Units: Data Profile 4 (DP04)",
                                     br(),
                                     "Home Value: Data Profile 4 (DP04)",
                                     br(),
                                     "Income: Data Profile 3 (DP03)",
                                     br(),
                                     "Industry: Data Profile 3 (DP03)",
                                     br(),
                                     "Mode Share: Data Profile 3 (DP03)",
                                     br(),
                                     "Monthly Rent: Data Profile 4 (DP04)",
                                     br(),
                                     "Occupation: Data Profile 3 (DP03)",
                                     br(),
                                     "Race: Data Profile 5 (DP05)",
                                     br(),
                                     "Vehicles Available: Data Profile 4 (DP04)",
                                     br()
                            ) # end of Data tabset panel
                            
                                                        
                    ) # end of NavBar Page
                ) # end of main panel
        ) # end of sidebar layout
    ) # end of main fluid page
) #end of shiny ui
