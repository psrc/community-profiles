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
                           id = "Navbar",
                           
                           home_tab_ui("home"),

                           tabPanel(icon("users"),
                                    "The person level metrics on this page cover the topics of Age, Race & Ethnicity, Health Coverage and Disability Status. ",
                                    "Data Profile 2 (DP02) includes information on People with Disabilites within a community, DP03 includes information of people's access to health coverage and DP05 includes details on Age and Race & Ethnicity. ",
                                    "Data profiles are a summarization of a variety of Census Detailed Tables contained within the American Community Survey datasets and are a great resource for high level statistics for a community however detailed information requires the use of specific ACS tables.",
                                    value = 'people',
                                    tabsetPanel(
                                      id = 'tab_people',
                                      tabPanel("Age",
                                               value = 'age',
                                               fluidRow(
                                                 column(width = 6, plotlyOutput("plot_age")),
                                                 column(width = 6, leafletOutput("age_map"))
                                               ), # end of fluid row
                                               fluidRow(
                                                 column(width = 12,hr(),DT::dataTableOutput("table_age"))
                                               ) # end of fluid Row
                                      ), # end of age tab panel
                                      
                                      tabPanel("Race & Ethnicity",
                                               value = 're',
                                               fluidRow(
                                                 column(width = 6, plotlyOutput("plot_race")),
                                                 column(width = 6, leafletOutput("race_map"))
                                               ), # end of fluid row
                                               fluidRow(
                                                 column(width = 12,hr(),DT::dataTableOutput("table_race"))
                                               ) # end of fluid Row
                                      ), # end of race tab panel
                                      
                                      
                                      
                                      tabPanel("Health Coverage",
                                               value = 'health',
                                               fluidRow(
                                                 column(width = 6, plotlyOutput("plot_health")),
                                                 column(width = 6, leafletOutput("health_map"))
                                               ), # end of fluid row
                                               fluidRow(
                                                 column(width = 12,hr(),DT::dataTableOutput("table_health"))
                                               ) # end of fluid Row
                                      ), # end of Health Coverage Tab Panel
                                      
                                      tabPanel("People with a Disability",
                                               value = 'disability',
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
                                    value = 'housing',
                                    tabsetPanel(
                                      id = 'tab_housing',
                                      tabPanel("Housing Units",
                                               value = 'units',
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

                           tabPanel("RDI",
                                    rdi_tab_ui("rdi")
                           ),
                           
                           tabPanel(icon("briefcase"),
                                    briefcase_tab_ui("briefcase")
                                    ),
                           
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
                           
                           source_tab_ui("source"),
                          
                           footer = p("Click on the icon at the top of the page that corresponds to the metrics that you are interested in 
                                      and you will get access to those measures. 
                                      If you have any questions about the data or are curious what else we might have, please click",
                                    tags$a(class = "source_url", href="https://www.psrc.org/contact/information-center", "here", target="_blank"),
                                    " and we will be happy to help.",
                                    style = "font-size: 10pt; margin-top: 2rem;")
                ) # end of NavBar Page
      ) # end of main panel
    ) # end of sidebar layout
  ) # end of main fluid page
) #end of shiny ui
