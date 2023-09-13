# Display  (Wrench icon)

wrench_tab_ui <- function(id) {
  ns <- NS(id)
  
  tip_intro <- div(
    p("The TIP provides a summary of current transportation projects underway within King, Pierce, Snohomish, and Kitsap counties. 
           These projects are funded with federal, state and local funds, including the most recent federal grants awarded through PSRC. ",
        style = "margin-bottom: .5rem;"),
    p("The TIP spans a four-year period and must be updated at least every two years. After public review and comment, 
           the TIP is approved by the Regional Council's Transportation Policy and Executive Boards before being submitted for further 
           approvals to the Governor and ultimately the U.S. Department of Transportation.")
  )
  
  rtp_intro <- div(
    p("Larger scale regional investments planned through 2050 are included in the RTP on the Regional Capacity Projects list.",
        style = "margin-bottom: .5rem;"),
    p("Regional Capacity Projects are those projects adding capacity to the regional system above a pre-determined threshold, 
        and include roadway, transit, bicycle/pedestrian and other project types. Projects meeting this threshold must be approved
        on the list before proceeding towards funding and implementation. Projects that are below this threshold are considered
        programmatic in the plan and are able to pursue funding and implementation with no further actions.",
        style = "margin-bottom: .5rem;"),
    p("As part of the update, projects are requested to be either in the financially constrained plan or in the Unprogrammed 
        portion of the plan.")
    )
  
  div(
    tabsetPanel(
      id = ns('tabset'),
      tip_rtp_tab_ui(id = ns('tip'), 
                     subtab_title = "Transportation Improvement Program", 
                     intro_text = tip_intro, 
                     value = NULL),
      tip_rtp_tab_ui(id = ns('rtp'), 
                     subtab_title = "Regional Transportation Plan", 
                     intro_text = rtp_intro, 
                     value = NULL)
    )
  )
  
}

wrench_tab_server <- function(id, place) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns

    tip_rtp_tab_server(id = 'tip', 
                       place = reactive(place()), 
                       plan_year = "2021-2024 TIP", 
                       type = 'tip')
    
    tip_rtp_tab_server(id = 'rtp', 
                       place = reactive(place()), 
                       plan_year = rtp.status, 
                       type = 'rtp')
    
  }) # end moduleServer
  
}