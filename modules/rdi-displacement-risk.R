# Display Displacement Risk 

rdi_disp_risk_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(title = "Displacement Risk",
           div(style = "padding-top: 1rem;",
               fluidRow(
                 column(width = 6, 
                        p("Hold for description")
                 ),
                 column(width = 6, 
                        leafletOutput(ns("map"))
                 ),
                 style = "margin-top: 1rem;"
               ), # end fluidRow 
               fluidRow(
                 column(width = 12, 
                        DTOutput(ns("table")
                        )
                 )
               ) # end fluidRow
           ) # end div
           
  )# end tabpanel
}

rdi_disp_risk_server <- function(id, shape, place, disp_risk_shape) {
  
  moduleServer(id, function(input, output, session) { 
    ns <- session$ns
    
    vals <- reactiveValues(source = 'Sources: ',
                           id_cols = c('chas_year', 'geography_name', 'description'))
    
    output$tableui <- renderUI({
      div(
        withSpinner(
          DTOutput(ns("table")),
          type = 5,
          color = psrc_colors$pgnobgy_10[sample.int(10, 1)]
        ),
        style = 'margin-top: 1rem'
      )
      
    })
    
    
    data <- reactive({
      
    })
    
    table_data <- reactive({
      # pull (currently from SQLite)
      # data in wide for display in table
      
      select_rename_cols <- function(df) {
        place_name <- unique(df$planning_geog)
        
        df %>% 
          select(planning_geog, race_ethnicity_label, contains('share'), ends_with('All'), starts_with('reliability')) %>%
          select(planning_geog, race_ethnicity_label, starts_with('estimate'), starts_with('reliability')) %>% 
          rename_with(~paste0(paste0(place_name, "_"), .x, recycle0 = TRUE), 
                      setdiff(colnames(.), c('planning_geog', 'race_ethnicity_label'))) %>% 
          select(-planning_geog)
      }
      
      df <- read.dt.disprisk(type = 'table', "acs5_2022_B03002_tract_juris_split_summary")

      juris <- df %>% 
        filter(planning_geog == place()) %>% 
        select_rename_cols()
      
      region <- df %>% 
        filter(planning_geog == 'Region') %>% 
        select_rename_cols()
      
      t <- left_join(juris, region, by = 'race_ethnicity_label')
    })
    
    plot_data <- reactive({
      # data in long form for plotting
      
      # data()
      
    })
    
    place_name <- reactive({
      place()
    })
    
    container <- reactive({
      # custom container for DT
      
       sel_cols <-  c(paste(c('Lower', 'Moderate', 'Higher'), "Risk"), "All", paste(c('Lower', 'Moderate', 'Higher'), "Reliability"))

        htmltools::withTags(table(
          class = 'display',
          thead(
            tr(
              th(rowspan = 2, 'Race/Ethnicity'),
              th(class = 'dt-center', colspan = 7, place_name()),
              th(class = 'dt-center', colspan = 7, 'Region')
            ),
            tr(
              lapply(rep(sel_cols, 2), th)
            )
          )
        ))
      
    })
    
    output$table <- renderDT({
      # table display
      t <- table_data()

      main_cols <- colnames(t)[grep(".*_estimate.*", colnames(t))]
      sel_cols <- str_subset(main_cols, ".*_estimate_share_.*")
      sel_cols2 <- str_subset(main_cols, ".*All")
      invis_cols <- str_subset(colnames(t), ".*reliability.*")
      
      round_to_tens <- partial(round, digits = -1)
      
      t <- t %>% 
        mutate(across(sel_cols2, round_to_tens))
      
      source <- "Sources: American Community Survey (ACS) 2018-2022 Table B03002, Puget Sound Regional Council (PSRC)"
      
      tooltip_js <-  c("function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {",
                                  "const tableCol = [1, 2, 3, 5, 6, 7];",
                                  "const dataCol = [5, 6, 7, 12, 13, 14];",
                                  "for(i = 0; i < tableCol.length; i++) {",
                                  "$('td:eq('+tableCol[i]+')', nRow).attr('data-title', aData[dataCol[i]]);",
                                  "}",
                        "}")
      
      datatable(t,
                container = container(),
                rownames = FALSE,
                options = list(dom = 'tipr',
                               columnDefs = list(list(className = 'dt-center', targets = c(1:4, 8:11)),
                                                 list(visible = FALSE, targets = c(5:7, 12:14))),
                               rowCallback = JS(tooltip_js)
                               ),
                caption = htmltools::tags$caption(
                  style = 'caption-side: bottom; text-align: right;',
                  htmltools::em(source))) %>% 
        formatPercentage(sel_cols, 1)
      
      
      # https://stackoverflow.com/questions/40224925/r-shiny-mouseover-to-all-table-cells/40634033#40634033
      # https://willdebras.github.io/posts/tooltips/
      
      # can hover and have tooltip? yes
      # can hide columns but still reference them in tooltip? yes
      # how to have tooltip for each cell in table?
      
      # td (table html ref)
      # eq(3) = third column
      # nRow = each row
      #$('td', nRow) all cells for each row
      # .attr('data-title', full_text) apply data-title attribute css to text
      # datatable(palmerpenguins::penguins,
      #           options = list(columnDefs = list(list(visible = FALSE, targets = c(5, 6, 7))),
      #                          rowCallback = JS(
      #                            "function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {",
      #                            "var full_text = aData[0] + ','+ aData[1] + ',' + aData[2] + ','+ aData[7];",
      #                            "$('td', nRow).attr('data-title', full_text);",
      #                            "}")
      #           )
      # )
      
    })
    
   
    
    
    map_data <- reactive({
      s <- shape %>% filter(geog_name == place())
    })
    
    output$map <- renderLeaflet({
      
      m <- create_displacement_risk_map(shape_tract = disp_risk_shape,
                                        shape_place = map_data(), 
                                        title = paste('Displacement Risk'))

    })
    
    
  }) # end moduleServer
  
}