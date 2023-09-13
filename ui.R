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
                                    value = "people",
                                    people_tab_ui("people")),
                           
                           tabPanel(icon("home"),
                                    value = 'housing',
                                    house_tab_ui("house")),

                           tabPanel("RDI",
                                    rdi_tab_ui("rdi")),
                           
                           tabPanel(icon("briefcase"),
                                    briefcase_tab_ui("briefcase")),
                           
                           tabPanel(icon("car"),
                                    car_tab_ui("car")),
                           
                           tabPanel(icon("wrench"),
                                    wrench_tab_ui("wrench")),
                           
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
