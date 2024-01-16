# Function to assemble Rental Affordability table

# source('modules/function-query-sqlite-chas.R')

create_rental_affordability_table <- function(juris = c('place', 'county', 'region')) {
  # gather tables T8, T15C, and T14B to create formatted Rental Affordability table
  
  chas_tables <- c('T8', 'T15C', 'T14B')
  ifelse(juris == 'place', j <- 'place', j <- 'county')
  dfs <- gather_tables(j, chas_tables)

  # Assemble Table ----
  
  desc <- c('Extremely Low Income (≤30% AMI)', 
            'Very Low Income (30-50% AMI)', 
            'Low Income (50-80% AMI)', 
            'Moderate Income (80-100% AMI)', 
            'Greater than 100% of AMI', 
            'All')

  desc2 <- c(desc[1:3], 'Greater than 80% AMI', 'All')
  
  # Table 8 
  t8_head <- c(69, 82, 95, 108, 121, 68)
  names(t8_head) <- desc
  
  t8 <- dfs$T8[sort %in% t8_head, ]
  t8 <- t8[, `:=`(sort = factor(sort, levels = t8_head), col_desc = 'renter_hh_income')][order(sort)]
  t8$description <- names(t8_head)[t8$sort]
  
  # Table 15C
  t15c_head <- c(4, 25, 46, 67, 3)
  t15c_desc <- c(desc[1:4], desc[6])
  names(t15c_head) <- t15c_desc
  t15c <- dfs$T15C[sort %in% t15c_head, ]
  t15c <- t15c[, `:=` (sort = factor(sort, levels = t15c_head), col_desc = 'rental_unit_affordability')][order(sort)]
  t15c$description <- names(t15c_head)[t15c$sort]
  
  # Table 14B
  t14b_head <- c(4, 8, 12, 16, 3)
  t14b_desc <- c(desc[1:4], desc[6])
  names(t14b_head) <- t14b_desc
  t14b <- dfs$T14B[sort %in% t14b_head, ]
  t14b <- t14b[, `:=` (sort = factor(sort, levels = t14b_head), col_desc = 'vacant_rental_units')][order(sort)]
  t14b$description <- names(t14b_head)[t14b$sort]

  # select common columns
  cols <- c('variable_name', 'sort','chas_year', 'geography_name', 'estimate', 'moe',  'col_desc', 'description')
  ra_dfs <- map(list(t8, t15c, t14b), ~.x[, ..cols])
  df <- rbindlist(ra_dfs)
  
  # aggregate two highest categories
  df[description %in% c('Moderate Income (80-100% AMI)','Greater than 100% of AMI'), description := 'Greater than 80% AMI']

  df <- df[, .(estimate = sum(estimate), moe = moe_sum(moe, estimate)), by = c('chas_year', 'geography_name', 'col_desc', 'description')]
  # df <- df[, .(estimate = sum(estimate)), by = c('chas_year', 'geography_name', 'col_desc', 'description')]
  
  ## Format Table ----

  if(juris == 'region') {
    # aggregate counties to region

    df <- df[, .(estimate = sum(estimate), moe = moe_sum(moe, estimate)), by = c('chas_year', 'description', 'col_desc')
             ][, geography_name := 'Region'] 
  }
  
  
  # sum Rental Unit Affordability & Vacant Rental Units for Total Rental Units by geography & Income/Affordability
  df_ra <- df[col_desc %in% c("rental_unit_affordability", "vacant_rental_units") , .(estimate = sum(estimate), moe = moe_sum(moe, estimate)), 
              by = c('chas_year', 'geography_name', 'description')
              ][, col_desc := 'rental_units']
  
  d <- rbindlist(list(df_ra, df[col_desc %in% c('renter_hh_income', 'rental_units')]), use.names=TRUE) 
  
  # set-up denominators
  d_tot <- d[description == 'All', ][, description := NULL]
  setnames(d_tot, c('estimate', 'moe'), c('estimate_denom', 'moe_denom'))
 
  dt <- merge(d, d_tot, by = c('chas_year', 'geography_name', 'col_desc'))
  dt[, share := estimate/estimate_denom][, share_moe := moe_prop(num = estimate, denom = estimate_denom, moe_num = moe, moe_denom = moe_denom)]
  
  # Exclude MOE until ready
  cols <- c('chas_year', 'geography_name', 'description', 'col_desc', 'estimate', 'share')
  dt <- dt[, ..cols]

  # pivot wider
  dt_wide <- dcast.data.table(dt, chas_year + geography_name + description ~ col_desc, value.var = c('estimate', 'share'))
  
  new_names <- c('rental_units', 'renter_hh_income')
  new_names <- c(new_names, paste0(new_names, '_share'))
  setnames(dt_wide, colnames(dt_wide)[4:7], new_names)
  setcolorder(dt_wide, c('chas_year', 'geography_name', 'description', new_names))

  # reorder rows
  dt_wide <- dt_wide[, description := factor(description, levels = desc2)][order(description)]
  
  return(dt_wide)
}

# x <- create_rental_affordability_table(juris = 'place')