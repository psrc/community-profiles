# Function to assemble Tenure by R/E table
# written by Eric Clute

# source('modules/function-query-sqlite-chas.R')

create_tenure_table <- function(juris = c('place', 'county', 'region')) {

  chas_table_codes <- 'T9'
  ifelse(juris == 'place', j <- 'place', j <- 'county')
  dfs <- gather_tables(j, chas_table_codes)

  re_order <- c('American Indian and Alaska Native', 
                'Asian', 
                'Black or African American', 
                'Hispanic or Latino (of any race)', 
                'Pacific Islander',
                'Other',
                'People of Color (POC)',
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
     ][, description := fcase(grepl("^American Indian ", description), "American Indian and Alaska Native",
                              grepl("^Asian ", description), "Asian",
                              grepl("^Black ", description), "Black or African American",
                              grepl("^Hispanic, any race", description), "Hispanic or Latino (of any race)",
                              grepl("^other ", description), "Other",
                              grepl("^Pacific ", description), "Pacific Islander",
                              grepl("^White ", description), "White",
                              grepl("^All", description), "All")]

  # POC
  poc <- df[!(description %in% c('All', 'White')), .(estimate = sum(estimate), 
                                                     moe = moe_sum(moe = moe, estimate = estimate), 
                                                     description = 'People of Color (POC)'), 
     by = c('chas_year', 'geography_name', 'col_desc')]
  
  df <- rbindlist(list(df, poc), use.names=TRUE, fill=TRUE)
  
  ## Format Table ----

  if(juris == 'region') {
    # aggregate counties to region

    df <- df[, .(estimate = sum(estimate), moe = moe_sum(moe = moe, estimate = estimate)), 
             by = c('variable_name', 'sort', 'chas_year', 'description', 'col_desc')
    ][, geography_name := 'Region'] 
  }

  # Calculate Shares, MOE included
  df_denom <- df[, .(estimate_denom = sum(estimate), moe_denom = moe_sum(moe, estimate)), 
                 by = c("chas_year", "geography_name", "description")]
  df <- merge(df, df_denom, by = c("chas_year", "geography_name", "description"))
  
  df[, share := estimate/estimate_denom
     ][, share_moe := moe_prop(num = estimate, denom = estimate_denom, moe_num = moe, moe_denom = moe_denom)]
  
  # Exclude MOE until ready
  cols <- c('chas_year', 'geography_name', 'description', 'col_desc', 'estimate', 'estimate_denom', 'share')
  df <- df[, ..cols]
  
  # pivot wider
  df <- dcast.data.table(df, chas_year + geography_name + description + estimate_denom ~ col_desc, value.var = c('estimate', 'share'))
  setnames(df, colnames(df), c(cols[1:3], 'all_units', 'owner_occupied', 'renter_occupied', 'owner_share', 'renter_share'))

  # reorder rows
  df <- df[, description := factor(description, levels = re_order)][order(description)]
  
  
  return(df)
}

# x <- create_tenure_table(juris = 'region')
