# Function to assemble Tenure by R/E table
# written by Eric Clute

# source('modules/function-query-sqlite-chas.R')

create_tenure_table <- function(juris = c('place', 'region')) {

  chas_table_codes <- 'T9'
  ifelse(juris == 'place', j <- 'place', j <- 'county')
  dfs <- gather_tables(j, chas_table_codes)
  
  re_order <- c('American Indian or Alaskan Native', 
                'Asian', 
                'Black or African American', 
                'Hispanic or Latino (of any race)', 
                'Pacific Islander',
                'Other', 
                'White',
                'All')
  
  # Assemble Table ----
  
  # Table 9 Owner-Occupied 
  oo_head <- c(18,13,8,28,33,23,3,2)
  
  oo <- dfs$T9[sort %in% oo_head, ]
  oo <- oo[, `:=`(sort = factor(sort, levels = oo_head), col_desc = 'owner_occupied')][order(sort)]
  
  # Table 9 Renter-Occupied
  ro_head <- c(54,49,44,64,69,59,39,38)
  
  ro <- dfs$T9[sort %in% ro_head, ]
  ro <- ro[, `:=`(sort = factor(sort, levels = ro_head), col_desc = 'renter_occupied')][order(sort)]
  
  # select common columns
  cols <- c('variable_name', 'sort','chas_year', 'geography_name', 'estimate', 'moe', 'race_ethnicity', 'col_desc')
  dfs <- map(list(oo, ro), ~.x[, ..cols])
  df <- rbindlist(dfs)
  
  # Format Race/Ethnicity data
  df[, description := race_ethnicity
     ][, description := fcase(grepl("^American Indian ", description), "American Indian or Alaskan Native",
                              grepl("^Asian ", description), "Asian",
                              grepl("^Black ", description), "Black or African American",
                              grepl("^Hispanic, any race", description), "Hispanic or Latino (of any race)",
                              grepl("^other ", description), "Other",
                              grepl("^Pacific ", description), "Pacific Islander",
                              grepl("^White ", description), "White",
                              grepl("^All", description), "All")]
  
  ## Format Table ----
  
  if(juris == 'region') {
    # aggregate counties to region
    
    df <- df[, .(estimate = sum(estimate)), by = c('variable_name', 'sort', 'chas_year', 'description', 'col_desc')
    ][, geography_name := 'Region'] 
  }
  
  # pivot wider
  df <- dcast.data.table(df, chas_year + geography_name + description ~ col_desc, value.var = 'estimate')
  
  # reorder rows
  df <- df[, description := factor(description, levels = re_order)][order(description)]
  
  # order columns
  setcolorder(df, c('chas_year', 'geography_name', 'description', 'owner_occupied', 'renter_occupied'))
  
  # Calculate Table ----
  df[, all_units := owner_occupied + renter_occupied
     ][, `:=` (renter_share = renter_occupied/all_units,
                owner_share = owner_occupied/all_units)]
  
  return(df)
}