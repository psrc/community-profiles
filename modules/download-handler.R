# Download data links (on sidebar)

download_data_ui <- function(id) {
  ns <- NS(id)
  
  div(
    div(
      downloadLink(ns('dp'), 
                   label = "Download Data Profiles in Excel Format"),
      style = "margin-bottom: .2rem;"
    ),
    downloadLink(ns('chas'),
                 label = "Download CHAS Data in Excel Format")
  )
  
}

download_data_server <- function(id, place, year) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    filenames <- reactive({
      data_profile <- paste0(tolower(str_replace_all(year()," ","-")),"-",tolower(str_replace_all(place()," ","-")),".xlsx")
      
      chas_data <- paste0("2015-2019-chas-data-", tolower(place()), ".xlsx")
      
      return(list(dp = data_profile, cd = chas_data))
    })
    
    output$dp <- downloadHandler(
      filename =  function() {
        filenames()$dp
      },
      content = function(file) {
        file.copy(here(paste0("data-profiles/", filenames()$dp)), file)
      },
      contentType = "application/Excel"
    )
    
    output$chas <- downloadHandler(
      filename =  function() {
        filenames()$cd
      },
      content = function(file) {
        file.copy(here(paste0("data-profiles-chas/", filenames()$cd)), file)
      },
      contentType = "application/Excel"
    )
  
  }) # end moduleServer
  
}