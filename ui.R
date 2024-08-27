# User Interface for a Place Selection with a map returned for that place.

shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(id = "sidebar",
                   div(img(src="psrc-logo.png", width = "70%"), style = "text-align: center; margin-bottom: 2rem;"),
                 
                   selectInput("Place",
                               "Select your Community:",
                               list("City" = data_places, "County"= data_counties), 
                               selected = "Bellevue"),
                   selectInput("Year",
                               "Select American Community Survey Data:",
                               data_years),

                   juris_profile_ui('profile'),
                   download_data_ui('download'),
                  
                   width = 3),
      
      mainPanel(shinyjs::useShinyjs(), 
                id ="Main", 
                width = 9,
                navbarPage(title = "", 
                           theme = "styles.css", 
                           windowTitle = "PSRC Community Profiles",
                           id = "Navbar",
                           
                           home_tab_ui("home"),

                           tabPanel(icon("users"),
                                    value = "people",
                                    people_tab_ui("people")),
                           
                           tabPanel(icon("home"),
                                    value = 'housing',
                                    house_tab_ui("house")),

                           tabPanel("RDI",
                                    value = 'rdi',
                                    rdi_tab_ui("rdi")),
                           
                           tabPanel(icon("briefcase"),
                                    briefcase_tab_ui("briefcase")),
                           
                           tabPanel(icon("car"),
                                    car_tab_ui("car")),
                           
                           tabPanel(icon("wrench"),
                                    wrench_tab_ui("wrench")),
                           
                           source_tab_ui("source"),
                          
                           footer = p("If you have any questions about the data or are curious what other data we might have, please click",
                                    tags$a(class = "source_url", href="https://www.psrc.org/contact/information-center", "here", target="_blank"),
                                    " and we will be happy to help.",
                                    style = "font-size: 10pt; margin-top: 2rem;")
                ) # end of NavBar Page
      ) # end of main panel
    ) # end of sidebar layout
  ) # end of main fluid page
) #end of shiny ui
