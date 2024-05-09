library(tidyverse)
library(openxlsx)

scripts <- c('cost-burden', 'income', 'rental-affordability', 'tenure')
scripts <- paste0("function-", scripts, ".R")

source('modules/function-query-sqlite-chas.R')

walk(scripts, ~source(file.path('modules', .x)))
rm(list = str_subset(ls(), "_dt_"))

geogs <- c('place', 'county', 'region')

funcs <- str_subset(ls(), "create")

dfs <- list()
for(f in funcs) {
  if(str_detect(f, "cost_burden") | str_detect(f, "income")) {
    for(type in c('e', 's')) {
      df <- map(geogs, ~pluck(do.call(f, list(.x)), type)) |> rbindlist()
      
      ifelse(type == 'e', t <- 'estimate', t <- 'share') 
      
      # set name
      element_name <- str_replace_all(f, "create_", "") |> 
        str_replace_all("_table", "") |>
        paste0("_", t)
      
      # add to list
      dfs[[element_name]] <- df
      
    }
  } else {
    
    df <- map(geogs, ~do.call(f, list(.x))) |> rbindlist()
    
    # set name
    element_name <- str_replace_all(f, "create_", "") |>
      str_replace_all("_table", "")
    
    # add to list
    dfs[[element_name]] <- df
  }
  
} 

# munge ----
## clean column names
for(i in 1:length(dfs)) {
  n <- names(dfs[i])
  
  if(str_detect(n, "cost")) {
    setnames(dfs[[i]], c('chas_year', 'geography_name', 'tenure', 'race_ethnicity'), c('Year', 'Jurisdiction', 'Tenure', 'Race or Ethnicity'))

  } else if(str_detect(n, "income")) {
    setnames(dfs[[i]], c('chas_year', 'geography_name', 'tenure', 'race_ethnicity_grp'), c('Year', 'Jurisdiction', 'Tenure', 'Race or Ethnicity'))

  } else if(str_detect(n, "rental")) {
    setnames(dfs[[i]], c('chas_year', 'geography_name', 'description'), c('Year', 'Jurisdiction', 'Income'))
    colnames(dfs[[i]]) <- str_replace_all(colnames(dfs[[i]]), "hh", "household")
    colnames(dfs[[i]]) <- str_replace_all(colnames(dfs[[i]]), "_", " ")

  } else if(str_detect(n, "tenure")) {
    setnames(dfs[[i]], c('chas_year', 'geography_name', 'description'), c('Year', 'Jurisdiction', 'Race or Ethnicity'))
    colnames(dfs[[i]]) <- str_replace_all(colnames(dfs[[i]]), "_", " ")
    
  }
  
  if(str_detect(n, "rental") == T | str_detect(n, "tenure") == T) {
    colnames(dfs[[i]]) <- str_to_title(colnames(dfs[[i]]))
  }
  
}

# exclude CDPs
chas_geogs <- unique(dfs[[1]]$Jurisdiction)
cdps <- str_subset(chas_geogs, "CDP")
juris <- setdiff(chas_geogs, cdps)

chas_year <- unique(dfs[[1]]$Year)

# create function
# format excel 
# create_ofm_hct_summary_spreadsheet https://github.com/psrc/centers-monitoring-data-tools/blob/main/functions.R

create_cost_burden_income_sheet <- function(data, wb, sheetname, numFmt) {
  nr <- nrow(data) + 1
  nc <- ncol(data)
  
  addWorksheet(wb, sheetname)
  df_style <- createStyle(fontName = 'Poppins', fontSize = 9.5)
  head_style <- createStyle(halign = 'center')
  head_style2 <- createStyle(halign = 'left')
  est_style <- createStyle(halign = 'center', numFmt = numFmt)
  
  writeData(wb, sheetname, data, headerStyle = head_style)
  
  addStyle(wb, sheetname, style = df_style, rows = 1:nr, cols = 1:nc, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheetname, style = est_style, rows = 2:nr, cols = 5:nc, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheetname, style = head_style2, rows = 1, cols = 1:4, gridExpand = TRUE, stack = TRUE)
}

create_rental_tenure_sheet <- function(data, wb, sheetname) {
  nr <- nrow(data) + 1
  nc <- ncol(data)
  
  addWorksheet(wb, sheetname)
  df_style <- createStyle(fontName = 'Poppins', fontSize = 9.5)
  head_style <- createStyle(halign = 'center')
  head_style2 <- createStyle(halign = 'left')
  est_style <- createStyle(halign = 'center', numFmt = 'COMMA')
  per_style <- createStyle(halign = 'center', numFmt = 'PERCENTAGE')
  
  writeData(wb, sheetname, data, headerStyle = head_style)
  
  if(sheetname == 'Tenure') {
    per_start_nc <- 7
    est_end_nc <- 6
  } else {
    per_start_nc <- 6
    est_end_nc <- 5
  } 
 
  addStyle(wb, sheetname, style = df_style, rows = 1:nr, cols = 1:nc, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheetname, style = est_style, rows = 2:nr, cols = 4:est_end_nc, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheetname, style = per_style, rows = 2:nr, cols = per_start_nc:nc, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheetname, style = head_style2, rows = 1, cols = 1:3, gridExpand = TRUE, stack = TRUE)
}

# export each jurisdiction as separate workbook
for(j in setdiff(juris, 'Region')) {
  juris_df <- map(dfs, ~.x %>% filter(Jurisdiction %in% c(j, 'Region')))
  
  # create workbook
  wb <- createWorkbook()
  
  # create sheets
  create_cost_burden_income_sheet(data = juris_df$cost_burden_estimate, wb = wb, sheetname = "Cost Burden Estimate", numFmt = 'COMMA')
  create_cost_burden_income_sheet(data = juris_df$cost_burden_share, wb = wb, sheetname = "Cost Burden Share", numFmt = 'PERCENTAGE')
  create_cost_burden_income_sheet(data = juris_df$income_estimate, wb = wb, sheetname = "Income Estimate", numFmt = 'COMMA')
  create_cost_burden_income_sheet(data = juris_df$income_share, wb = wb, sheetname = "Income Share", numFmt = 'PERCENTAGE')
  create_rental_tenure_sheet(data = juris_df$rental_affordability, wb = wb, sheetname = "Rental Affordability")
  create_rental_tenure_sheet(data = juris_df$tenure, wb = wb, sheetname = "Tenure")
  
  # openXL(wb) # preview
  
  saveWorkbook(wb, file = here::here(file.path("data-profiles-chas", paste0(chas_year - 4, "-", chas_year, "-chas-data-", tolower(j), ".xlsx"))), overwrite = TRUE)
}

