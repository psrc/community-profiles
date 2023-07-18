# User Interface for a Place Selection with a map returned for that place.

shinyUI(
    fluidPage(
      sidebarLayout(
        sidebarPanel(id = "sidebar",
            div(img(src="psrc-logo.png", width = "70%", height = "70%", style = "padding-top: 5px")),
            br(),
            selectInput("Place","Select your Community:",list("City" = data_places, "County"=data_counties), selected = "Bellevue"),
            selectInput("Year","Select American Community Survey Data:",data_years),
            textOutput("Population"),
            textOutput("POCShare"),
            textOutput("MedianAge"),
            textOutput("DisabledShare"),
            textOutput("MedianIncome"),
            textOutput("AvgHHSize"),
            textOutput("OwnShare"),
            textOutput("UnempRate"),
            textOutput("AvgTT"),
            h3("Regional Definitions:"),
            textOutput("place_rgeo"),
            textOutput("place_airaff"),
            br(),
            downloadLink('downloadData', label = "Download Data Profiles in Excel Format"),
            width = 3),
        mainPanel(shinyjs::useShinyjs(), id ="Main", width = 9,
            navbarPage(title = "", theme = "styles.css", windowTitle = "PSRC Community Profiles",
                       
                       tabPanel(icon("city"),
                                h1("Community Profiles"),
                                "As a State Data Center for the central Puget Sound region, PSRC keeps a complete inventory of data released from the U.S. Census Bureau. ",
                                "Cities and counties use it to track the well-being of children, families, and the elderly as well as to determine where to locate new public facilities. ",
                                "This portal includes demographic profiles on a variety of topics for all cities and towns in the PSRC region.",
                                hr(),
                                fluidRow(
                                    column(width=7, 
                                           "The data  on this dashboard is a snapshot of the information that is available from the US Census Bureau. ",
                                           "In order to view data for all the cities and towns in the Puget Sound region, we are limited to using American Community Survey (ACS) 5 year data. ",
                                           "For this reason, you will only find two non-overlapping sets of data on this dashboard. ",
                                           "By only including non-overlapping data, users can analyze the data for changing trends. ",
                                           "The data on this dashboard is divided into five categories:",br(),br(),
                                           icon("users"),"People Measures", br(),
                                           icon("home"),"Household & Housing Measures", br(),
                                           icon("briefcase"),"Job & Income Measures", br(),
                                           icon("car"),"Transportation Measures", br(),
                                           icon("wrench"),"Transportation Projects", br(),
                                           ),
                                    column(width=5, leafletOutput("place_map"))),
                                hr(),
                                fluidRow("Click on the icon at the top of the page that corresponds to the metrics that you are interested in and you will get access to those measures. If you have any questions about the data or are curious what else we might have, please click",
                                         tags$a(class = "source_url", href="https://www.psrc.org/contact-center/have-question?contact=9626&destination=node/9362&width=75%25&height=75%25&subject=Planning%20for%20Equity", "here", target="_blank")," and we will be happy to help.")
                                ), # end of Overview tabset panel
            
                        tabPanel(icon("users"),
                                 "The person level metrics on this page cover the topics of Age, Race & Ethnicity, Health Coverage and Disability Status. ",
                                 "Data Profile 2 (DP02) includes information on People with Disabilites within a community, DP03 includes information of people's access to health coverage and DP05 includes details on Age and Race & Ethnicity. ",
                                 "Data profiles are a summarization of a variety of Census Detailed Tables contained within the American Community Survey datasets and are a great resource for high level statistics for a community however detailed information requires the use of specific ACS tables.",
                                
                                tabsetPanel(
                                    
                                    tabPanel("Age",
                                             fluidRow(
                                                 column(width = 6, plotlyOutput("plot_age")),
                                                 column(width = 6, leafletOutput("age_map"))
                                                 ), # end of fluid row
                                             fluidRow(
                                                 column(width = 12,hr(),DT::dataTableOutput("table_age"))
                                                 ) # end of fluid Row
                                            ), # end of age tab panel
                                          
                                    tabPanel("Race & Ethnicity",
                                        fluidRow(
                                            column(width = 6, plotlyOutput("plot_race")),
                                            column(width = 6, leafletOutput("race_map"))
                                        ), # end of fluid row
                                        fluidRow(
                                            column(width = 12,hr(),DT::dataTableOutput("table_race"))
                                        ) # end of fluid Row
                                    ), # end of race tab panel
                                    

                                    
                                    tabPanel("Health Coverage",
                                             fluidRow(
                                                 column(width = 6, plotlyOutput("plot_health")),
                                                 column(width = 6, leafletOutput("health_map"))
                                             ), # end of fluid row
                                             fluidRow(
                                                 column(width = 12,hr(),DT::dataTableOutput("table_health"))
                                             ) # end of fluid Row
                                    ), # end of Health Coverage Tab Panel
                                    
                                    tabPanel("People with a Disability",
                                             fluidRow(
                                                 column(width = 6, plotlyOutput("plot_disability")),
                                                 column(width = 6, leafletOutput("disability_map"))
                                             ), # end of fluid row
                                             fluidRow(
                                                 column(width = 12,hr(),DT::dataTableOutput("table_disability"))
                                             ) # end of fluid Row
                                    ) # end of Disability Tab Panel
                                          
                                ) # end of Demographics tabset panel
                            ), # end of Demographics Tab Panel
            
                            tabPanel(icon("home"),
                                     "The housing and household level metrics on this page cover the topics of Housing Type, Home Values, Monthly Rental Cost and Home Ownership. ",
                                     "Data Profile 4 (DP04) includes a wealth of information on housing and household characteristics. ",
                                     "Data profiles are a summarization of a variety of Census Detailed Tables contained within the American Community Survey datasets and are a great resource for high level statistics for a community however detailed information requires the use of specific ACS tables.",
                                     
                                tabsetPanel(
                                
                                    tabPanel("Housing Units",
                                             fluidRow(
                                                 column(width = 6, plotlyOutput("plot_housingtype")),
                                                 column(width = 6, leafletOutput("housingtype_map"))
                                             ), # end of fluid row
                                             fluidRow(
                                                 column(width = 12,hr(),DT::dataTableOutput("table_housingtype"))
                                             ) # end of fluid Row
                                    ), # end of units tab panel
                                    
                                    tabPanel("Home Value",
                                             fluidRow(
                                                 column(width = 6, plotlyOutput("plot_homevalue")),
                                                 column(width = 6, leafletOutput("homevalue_map"))
                                             ), # end of fluid row
                                             fluidRow(
                                                 column(width = 12,hr(),DT::dataTableOutput("table_homevalue"))
                                             ) # end of fluid Row
                                    ), # end of Home Value tab panel
                         
                                    tabPanel("Monthly Rent",
                                             fluidRow(
                                                 column(width = 6, plotlyOutput("plot_monthlyrent")),
                                                 column(width = 6, leafletOutput("monthlyrent_map"))
                                             ), # end of fluid row
                                             fluidRow(
                                                 column(width = 12,hr(),DT::dataTableOutput("table_monthlyrent"))
                                             ) # end of fluid Row
                                    ), # end of Monthly Rent tab panel
                         
                                    tabPanel("Home Ownership",
                                             fluidRow(
                                                 column(width = 6, plotlyOutput("plot_ownership")),
                                                 column(width = 6, leafletOutput("ownership_map"))
                                             ), # end of fluid row
                                             fluidRow(
                                                 column(width = 12,hr(),DT::dataTableOutput("table_ownership"))
                                             ) # end of fluid Row
                                    ) # end of Home Ownership tab panel 
                         
                                ) # end of Housing tabset panel
                            ), # end of Housing Tab Panel

                            tabPanel(icon("briefcase"),
                                     "The job and income metrics on this page cover the topics of Educational Attainment, Occupation of residents, Industry of residents and Median Income for houesholds. ",
                                     "Job and income characteristics are summarized in Data Profile 3 (DP03) and Educational Attainment is included in DP02. ",
                                     "Data profiles are a summarization of a variety of Census Detailed Tables contained within the American Community Survey datasets and are a great resource for high level statistics for a community however detailed information requires the use of specific ACS tables.",
                                     
                                     tabsetPanel(
                                         
                                         tabPanel("Educational Attainment",
                                                  fluidRow(
                                                      column(width = 6, plotlyOutput("plot_edu")),
                                                      column(width = 6, leafletOutput("edu_map"))
                                                  ), # end of fluid row
                                                  fluidRow(
                                                      column(width = 12,hr(),DT::dataTableOutput("table_edu"))
                                                  ) # end of fluid Row
                                         ), # end of educational attainment tab panel
                                         
                                         tabPanel("Occupation",
                                                  fluidRow(
                                                      column(width = 6, plotlyOutput("plot_occupation")),
                                                      column(width = 6, leafletOutput("occupation_map"))
                                                  ), # end of fluid row
                                                  fluidRow(
                                                      column(width = 12,hr(),DT::dataTableOutput("table_occupation"))
                                                  ) # end of fluid Row
                                         ), # end of occupation tab panel
                                         
                                         tabPanel("Industry",
                                                  fluidRow(
                                                      column(width = 6, plotlyOutput("plot_industry")),
                                                      column(width = 6, leafletOutput("industry_map"))
                                                  ), # end of fluid row
                                                  fluidRow(
                                                      column(width = 12,hr(),DT::dataTableOutput("table_industry"))
                                                  ) # end of fluid Row
                                         ), # end of industry tab panel
                                         
                                         tabPanel("Income",
                                                  fluidRow(
                                                      column(width = 6, plotlyOutput("plot_income")),
                                                      column(width = 6, leafletOutput("income_map"))
                                                  ), # end of fluid row
                                                  fluidRow(
                                                      column(width = 12,hr(),DT::dataTableOutput("table_income"))
                                                  ) # end of fluid Row
                                         ) # end of income tab panel
                                         
                                     ) # end of jobs and income tabset panel
                            ), # end of jobs and income Tab Panel
                            
                            tabPanel(icon("car"),
                                     
                                     "The travel related metrics on this page cover the topics of Mode Share, Travel Time and Time of Departure for Work related travel as well as the number of Vehicles Available for households in the community. ",
                                     "Mode Share and Vehicle Availability are metrics from Data Profile 3 (DP03) and DP04 respectively. Detailed Travel Time comes from table B08303 and Departure Time is from table B08302. ",
                                     "Data profiles are a summarization of a variety of Census Detailed Tables contained within the American Community Survey datasets and are a great resource for high level statistics for a community however detailed information requires the use of specific ACS tables.",
                                     
                                tabsetPanel(
                                        tabPanel("Mode Share to Work",
                                                 fluidRow(
                                                     column(width = 6, plotlyOutput("plot_modes")),
                                                     column(width = 6, leafletOutput("modes_map"))
                                                 ), # end of fluid row
                                                 fluidRow(
                                                     column(width = 12,hr(),DT::dataTableOutput("table_modes"))
                                                 ) # end of fluid Row
                                        ), # end of mode share tab panel
                                                 
                                        tabPanel("Travel Time to Work",
                                                 fluidRow(
                                                     column(width = 6, plotlyOutput("plot_time")),
                                                     column(width = 6, leafletOutput("time_map"))
                                                 ), # end of fluid row
                                                 fluidRow(
                                                     column(width = 12,hr(),DT::dataTableOutput("table_time"))
                                                 ) # end of fluid Row
                                        ), # end of travel time tab panel
                                        
                                        tabPanel("Departure Time to Work",
                                                 fluidRow(
                                                     column(width = 6, plotlyOutput("plot_depart")),
                                                     column(width = 6, leafletOutput("depart_map"))
                                                 ), # end of fluid row
                                                 fluidRow(
                                                     column(width = 12,hr(),DT::dataTableOutput("table_depart"))
                                                 ) # end of fluid Row
                                        ), # end of departure time tab panel
                                        
                                        tabPanel("Vehicles Available",
                                                 fluidRow(
                                                     column(width = 6, plotlyOutput("plot_vehicles")),
                                                     column(width = 6, leafletOutput("vehicles_map"))
                                                 ), # end of fluid row
                                                 fluidRow(
                                                     column(width = 12,hr(),DT::dataTableOutput("table_vehicles"))
                                                 ) # end of fluid Row
                                        ) # end of Vehicle Availability tab panel 
                                                 
                                     ) # end of Transportation TabSet
                                     
                            ), # end of Transportation tabPanel

                       tabPanel(icon("wrench"),
                                
                                tabsetPanel(
                                    
                                    tabPanel("Transportation Improvement Program",
                                             fluidRow(
                                                 column(width = 6,
                                                        br(),
                                                        "The TIP provides a summary of current transportation projects underway within King, Pierce, Snohomish, and Kitsap counties. These projects are funded with federal, state and local funds, including the most recent federal grants awarded through PSRC. ",
                                                        br(),
                                                        br(),
                                                        "The TIP spans a four-year period and must be updated at least every two years. After public review and comment, the TIP is approved by the Regional Council's Transportation Policy and Executive Boards before being submitted for further approvals to the Governor and ultimately the U.S. Department of Transportation.",
                                                        br(),
                                                        br()
                                                 ),
                                                column(width = 6, leafletOutput("tip_map",height="400px"))
                                             ), # end of Fluid Row
                                             
                                             fluidRow(
                                                 column(width = 12, hr(), DT::dataTableOutput("table_tip")))
                                    ), # end of tip tab panel
                                    
                                    tabPanel("Regional Transportation Plan",
                                             fluidRow(
                                                 column(width = 6,
                                                        br(),
                                                        "Larger scale regional investments planned through 2050 are included in the RTP on the Regional Capacity Projects list.",
                                                        br(),
                                                        br(),
                                                        "Regional Capacity Projects are those projects adding capacity to the regional system above a pre-determined threshold, and include roadway, transit, bicycle/pedestrian and other project types. Projects meeting this threshold must be approved on the list before proceeding towards funding and implementation. Projects that are below this threshold are considered programmatic in the plan and are able to pursue funding and implementation with no further actions.",
                                                        br(),
                                                        br(),
                                                        "As part of the update, projects are requested to be either in the financially constrained plan or in the Unprogrammed portion of the plan.",
                                                        br(),
                                                        br()
                                                 ),
                                                 column(width = 6, br(), leafletOutput("rtp_map",height="400px"))
                                             ), # end of Fluid Row
                                             
                                             fluidRow(
                                                 column(width = 12, hr(), DT::dataTableOutput("table_rtp")))                                      
                                    ) # end of RTP tab panel
                                    
                                ) # end of Projects and Funding TabSet
                                
                       ), # end of Projects and Funding tabPanel 
                       
                       ## Insert RDI module----
                       tabPanel("RDI",
                                rdi_tab_ui("rdi")
                       ),
                       
                       
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
                                     "Departure Time to Work: Table B08302",
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
