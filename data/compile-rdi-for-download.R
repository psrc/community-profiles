library(tidyverse)

scripts <- c('cost-burden', 'income', 'rental-affordability', 'tenure')
# scripts <- c('rental-affordability', 'tenure') # doesn't have e or s 
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

# exclude CDPs
chas_geogs <- unique(dfs[[1]]$geography_name)
cdps <- str_subset(chas_geogs, "CDP")
juris <- setdiff(chas_geogs, cdps)

chas_year <- unique(dfs[[1]]$chas_year)

# export each jurisdiction as separate workbook
for(j in setdiff(juris, 'Region')) {
  chas_geog_df <- map(dfs, ~.x %>% filter(geography_name %in% c(j, 'Region')))
  openxlsx::write.xlsx(chas_geog_df, here::here(file.path("data-profiles-chas", paste0(chas_year - 4, "-", chas_year, "-chas-data-", j, ".xlsx"))))
}

